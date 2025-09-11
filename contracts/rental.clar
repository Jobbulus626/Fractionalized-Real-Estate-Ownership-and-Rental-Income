(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-already-exists (err u103))

(define-non-fungible-token property-share uint)

(define-map properties
    uint 
    {
        name: (string-ascii 64),
        total-shares: uint,
        rental-price: uint,
        available-shares: uint,
        tenant: (optional principal),
        rental-start: (optional uint),
        rental-end: (optional uint)
    }
)

(define-map share-ownership
    { property-id: uint, share-id: uint }
    principal
)

(define-map user-shares
    { owner: principal, property-id: uint }
    uint
)

(define-data-var next-property-id uint u1)
(define-data-var next-share-id uint u1)

(define-public (create-property (name (string-ascii 64)) (total-shares uint) (rental-price uint))
    (let ((property-id (var-get next-property-id)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-insert properties property-id
            {
                name: name,
                total-shares: total-shares,
                rental-price: rental-price,
                available-shares: total-shares,
                tenant: none,
                rental-start: none,
                rental-end: none
            }
        )
        (var-set next-property-id (+ property-id u1))
        (ok property-id)
    )
)

(define-public (mint-share (property-id uint))
    (let (
        (share-id (var-get next-share-id))
        (property (unwrap! (map-get? properties property-id) err-not-found))
        )
        (asserts! (> (get available-shares property) u0) err-not-found)
        (try! (nft-mint? property-share share-id tx-sender))
        (map-set share-ownership { property-id: property-id, share-id: share-id } tx-sender)
        (map-set properties property-id 
            (merge property { available-shares: (- (get available-shares property) u1) })
        )
        (map-set user-shares { owner: tx-sender, property-id: property-id }
            (+ (get-shares-owned tx-sender property-id) u1)
        )
        (var-set next-share-id (+ share-id u1))
        (ok share-id)
    )
)

(define-public (rent-property (property-id uint) (duration uint))
    (let (
        (property (unwrap! (map-get? properties property-id) err-not-found))
        (rental-cost (* (get rental-price property) duration))
        )
        (asserts! (is-none (get tenant property)) err-already-exists)
        (try! (stx-transfer? rental-cost tx-sender contract-owner))
        (map-set properties property-id
            (merge property {
                tenant: (some tx-sender),
                rental-start: (some stacks-block-height),
                rental-end: (some (+ stacks-block-height duration))
            })
        )
        (ok true)
    )
)

(define-public (distribute-rental-income (property-id uint))
    (let (
        (property (unwrap! (map-get? properties property-id) err-not-found))
        (total-shares (get total-shares property))
        (rental-price (get rental-price property))
        (share-amount (/ rental-price total-shares))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (try! (distribute-to-shareholders property-id share-amount))
        (ok true)
    )
)

(define-private (distribute-to-shareholders (property-id uint) (amount-per-share uint))
    (let ((shares-owned (get-shares-owned tx-sender property-id)))
        (if (> shares-owned u0)
            (stx-transfer? (* amount-per-share shares-owned) contract-owner tx-sender)
            (ok true)
        )
    )
)

(define-read-only (get-property (property-id uint))
    (map-get? properties property-id)
)

(define-read-only (get-shares-owned (owner principal) (property-id uint))
    (default-to u0 (map-get? user-shares { owner: owner, property-id: property-id }))
)

(define-read-only (get-share-owner (property-id uint) (share-id uint))
    (map-get? share-ownership { property-id: property-id, share-id: share-id })
)



(define-constant err-unauthorized (err u104))
(define-constant err-insufficient-shares (err u105))

(define-public (transfer-share 
    (property-id uint) 
    (share-id uint) 
    (recipient principal)
)
    (let (
        (current-owner (unwrap! (get-share-owner property-id share-id) err-not-found))
        (sender-shares (get-shares-owned tx-sender property-id))
    )
        (asserts! (is-eq current-owner tx-sender) err-unauthorized)
        (asserts! (> sender-shares u0) err-insufficient-shares)
        
        (try! (nft-transfer? property-share share-id tx-sender recipient))
        (map-set share-ownership { property-id: property-id, share-id: share-id } recipient)
        
        (map-set user-shares 
            { owner: tx-sender, property-id: property-id }
            (- sender-shares u1)
        )
        (map-set user-shares 
            { owner: recipient, property-id: property-id }
            (+ (get-shares-owned recipient property-id) u1)
        )
        (ok true)
    )
)


(define-constant err-invalid-price (err u106))

(define-public (update-rental-price 
    (property-id uint) 
    (new-price uint)
)
    (let ((property (unwrap! (map-get? properties property-id) err-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (> new-price u0) err-invalid-price)
        
        (map-set properties property-id
            (merge property { rental-price: new-price })
        )
        (ok true)
    )
)

(define-public (update-property-name 
    (property-id uint) 
    (new-name (string-ascii 64))
)
    (let ((property (unwrap! (map-get? properties property-id) err-not-found)))
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        (map-set properties property-id
            (merge property { name: new-name })
        )
        (ok true)
    )
)