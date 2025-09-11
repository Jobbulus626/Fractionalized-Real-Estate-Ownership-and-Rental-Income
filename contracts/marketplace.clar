(define-constant err-owner-only (err u200))
(define-constant err-not-found (err u201))
(define-constant err-invalid-amount (err u202))
(define-constant err-already-exists (err u203))
(define-constant err-unauthorized (err u204))
(define-constant err-insufficient-funds (err u205))
(define-constant err-bid-too-low (err u206))
(define-constant err-listing-expired (err u207))

;; Import the rental contract trait
;; (impl-trait .rental)

(define-map listings
    uint
    {
        seller: principal,
        property-id: uint,
        share-id: uint,
        price: uint,
        expires-at: uint,
        active: bool
    }
)

(define-map bids
    uint
    {
        bidder: principal,
        listing-id: uint,
        amount: uint,
        expires-at: uint
    }
)

(define-map listing-bids
    uint
    (list 10 uint)
)

(define-data-var next-listing-id uint u1)
(define-data-var next-bid-id uint u1)

(define-public (create-listing 
    (property-id uint) 
    (share-id uint) 
    (price uint) 
    (duration uint)
)
    (let (
        (listing-id (var-get next-listing-id))
        (rental-contract .rental)
    )
        (asserts! (> price u0) err-invalid-amount)
        (asserts! (> duration u0) err-invalid-amount)
        (asserts! (is-eq (some tx-sender)
        (contract-call? .rental get-share-owner property-id share-id)) err-unauthorized)

        (map-insert listings listing-id
            {
                seller: tx-sender,
                property-id: property-id,
                share-id: share-id,
                price: price,
                expires-at: (+ stacks-block-height duration),
                active: true
            }
        )
        (map-insert listing-bids listing-id (list))
        (var-set next-listing-id (+ listing-id u1))
        (ok listing-id)
    )
)

(define-public (place-bid (listing-id uint) (amount uint) (duration uint))
    (let (
        (listing (unwrap! (map-get? listings listing-id) err-not-found))
        (bid-id (var-get next-bid-id))
        (current-bids (default-to (list) (map-get? listing-bids listing-id)))
    )
        (asserts! (get active listing) err-not-found)
        (asserts! (< stacks-block-height (get expires-at listing)) err-listing-expired)
        (asserts! (> amount u0) err-invalid-amount)
        (asserts! (> duration u0) err-invalid-amount)
        (asserts! (not (is-eq tx-sender (get seller listing))) err-unauthorized)
        
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        (map-insert bids bid-id
            {
                bidder: tx-sender,
                listing-id: listing-id,
                amount: amount,
                expires-at: (+ stacks-block-height duration)
            }
        )
        
        (map-set listing-bids listing-id 
            (unwrap! (as-max-len? (append current-bids bid-id) u10) err-invalid-amount)
        )
        
        (var-set next-bid-id (+ bid-id u1))
        (ok bid-id)
    )
)

(define-public (accept-bid (listing-id uint) (bid-id uint))
    (let (
        (listing (unwrap! (map-get? listings listing-id) err-not-found))
        (bid (unwrap! (map-get? bids bid-id) err-not-found))
        (rental-contract .rental)
    )
        (asserts! (is-eq tx-sender (get seller listing)) err-unauthorized)
        (asserts! (get active listing) err-not-found)
        (asserts! (is-eq listing-id (get listing-id bid)) err-not-found)
        (asserts! (< stacks-block-height (get expires-at bid)) err-listing-expired)
        
        (try! (as-contract (stx-transfer? (get amount bid) tx-sender (get seller listing))))
        
        ;; (try! (contract-call? (contract-call? .rental transfer-share property-id:uint share-id:uint recipient:principal) 
        ;; transfer-share 
        ;;     (get property-id listing) 
        ;;     (get share-id listing) 
        ;;     (get bidder bid)
        ;; ))
        
        (map-set listings listing-id (merge listing { active: false }))
        
        ;; (try! (refund-other-bids listing-id bid-id))
        
        (ok true)
    )
)

(define-public (cancel-listing (listing-id uint))
    (let ((listing (unwrap! (map-get? listings listing-id) err-not-found)))
        (asserts! (is-eq tx-sender (get seller listing)) err-unauthorized)
        (asserts! (get active listing) err-not-found)
        
        (map-set listings listing-id (merge listing { active: false }))
        (try! (refund-all-bids listing-id))
        (ok true)
    )
)

(define-public (withdraw-bid (bid-id uint))
    (let ((bid (unwrap! (map-get? bids bid-id) err-not-found)))
        (asserts! (is-eq tx-sender (get bidder bid)) err-unauthorized)
        (asserts! (>= stacks-block-height (get expires-at bid)) err-listing-expired)
        
        (try! (as-contract (stx-transfer? (get amount bid) tx-sender (get bidder bid))))
        (ok true)
    )
)

(define-private (refund-other-bids (listing-id uint) (accepted-bid-id uint))
    (let ((bid-list (default-to (list) (map-get? listing-bids listing-id))))
        (fold refund-bid-if-not-accepted bid-list { accepted-id: accepted-bid-id, result: (ok true) })
    )
)

(define-private (refund-all-bids (listing-id uint))
    (let ((bid-list (default-to (list) (map-get? listing-bids listing-id))))
        (fold refund-single-bid bid-list (ok true))
    )
)

(define-private (refund-bid-if-not-accepted 
    (bid-id uint) 
    (context { accepted-id: uint, result: (response bool uint) })
)
    (if (and (is-ok (get result context)) (not (is-eq bid-id (get accepted-id context))))
        (match (map-get? bids bid-id)
            bid (match (as-contract (stx-transfer? (get amount bid) tx-sender (get bidder bid)))
                success { accepted-id: (get accepted-id context), result: (ok true) }
                error { accepted-id: (get accepted-id context), result: (err error) }
            )
            { accepted-id: (get accepted-id context), result: (get result context) }
        )
        context
    )
)

(define-private (refund-single-bid (bid-id uint) (prev-result (response bool uint)))
    (if (is-ok prev-result)
        (match (map-get? bids bid-id)
            bid (as-contract (stx-transfer? (get amount bid) tx-sender (get bidder bid)))
            (ok true)
        )
        prev-result
    )
)

(define-read-only (get-listing (listing-id uint))
    (map-get? listings listing-id)
)

(define-read-only (get-bid (bid-id uint))
    (map-get? bids bid-id)
)

(define-read-only (get-listing-bids (listing-id uint))
    (map-get? listing-bids listing-id)
)

(define-read-only (get-active-listings)
    (ok "Use get-listing with specific IDs to check active status")
)