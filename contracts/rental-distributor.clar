(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u300))
(define-constant err-not-found (err u301))
(define-constant err-invalid-amount (err u302))
(define-constant err-already-distributed (err u303))
(define-constant err-rental-active (err u304))
(define-constant err-paused (err u305))
(define-constant err-unauthorized (err u306))

(define-data-var contract-paused bool false)
(define-data-var total-distributed uint u0)

(define-map rental-periods
    { property-id: uint, period-start: uint }
    {
        period-end: uint,
        total-income: uint,
        distributed: bool,
        total-shares: uint
    }
)

(define-map shareholder-earnings
    { property-id: uint, shareholder: principal }
    uint
)

(define-public (register-rental-period 
    (property-id uint)
    (period-start uint)
    (period-end uint)
    (total-income uint)
)
    (let (
        (property (unwrap! (contract-call? .rental get-property property-id) err-not-found))
        (total-shares (get total-shares property))
    )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (not (var-get contract-paused)) err-paused)
        (asserts! (> total-income u0) err-invalid-amount)
        (asserts! (> period-end period-start) err-invalid-amount)
        
        (map-set rental-periods 
            { property-id: property-id, period-start: period-start }
            {
                period-end: period-end,
                total-income: total-income,
                distributed: false,
                total-shares: total-shares
            }
        )
        (ok true)
    )
)

(define-public (distribute-rental-income 
    (property-id uint)
    (period-start uint)
)
    (let (
        (period-data (unwrap! (map-get? rental-periods { property-id: property-id, period-start: period-start }) err-not-found))
        (total-income (get total-income period-data))
        (total-shares (get total-shares period-data))
        (income-per-share (/ total-income total-shares))
    )
        (asserts! (not (var-get contract-paused)) err-paused)
        (asserts! (not (get distributed period-data)) err-already-distributed)
        (asserts! (>= stacks-block-height (get period-end period-data)) err-rental-active)
        
        (try! (distribute-to-shareholders property-id income-per-share total-shares))
        
        (map-set rental-periods 
            { property-id: property-id, period-start: period-start }
            (merge period-data { distributed: true })
        )
        
        (var-set total-distributed (+ (var-get total-distributed) total-income))
        (ok true)
    )
)

(define-public (emergency-pause)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set contract-paused true)
        (ok true)
    )
)

(define-public (emergency-unpause)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set contract-paused false)
        (ok true)
    )
)

(define-private (distribute-to-shareholders (property-id uint) (income-per-share uint) (total-shares uint))
    (let ((share-range (generate-share-ids total-shares)))
        (get result (fold distribute-to-single-shareholder share-range 
            { property-id: property-id, income-per-share: income-per-share, result: (ok true) }
        ))
    )
)

(define-private (generate-share-ids (total-shares uint))
    (let ((all-ids (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32 u33 u34 u35 u36 u37 u38 u39 u40 u41 u42 u43 u44 u45 u46 u47 u48 u49 u50 u51 u52 u53 u54 u55 u56 u57 u58 u59 u60 u61 u62 u63 u64 u65 u66 u67 u68 u69 u70 u71 u72 u73 u74 u75 u76 u77 u78 u79 u80 u81 u82 u83 u84 u85 u86 u87 u88 u89 u90 u91 u92 u93 u94 u95 u96 u97 u98 u99 u100)))
        (get result (fold filter-valid-share-id all-ids { max-shares: total-shares, result: (list) }))
    )
)

(define-private (filter-valid-share-id 
    (share-id uint) 
    (acc { max-shares: uint, result: (list 100 uint) })
)
    (if (<= share-id (get max-shares acc))
        (let ((new-result (as-max-len? (append (get result acc) share-id) u100)))
            (match new-result
                success { 
                    max-shares: (get max-shares acc),
                    result: success
                }
                { 
                    max-shares: (get max-shares acc),
                    result: (get result acc)
                }
            )
        )
        { 
            max-shares: (get max-shares acc),
            result: (get result acc)
        }
    )
)

(define-private (distribute-to-single-shareholder 
    (share-id uint) 
    (context { property-id: uint, income-per-share: uint, result: (response bool uint) })
)
    (if (is-ok (get result context))
        (let (
            (property-id (get property-id context))
            (income-per-share (get income-per-share context))
        )
            (match (contract-call? .rental get-share-owner property-id share-id)
                shareholder (let (
                    (shares-owned (contract-call? .rental get-shares-owned shareholder property-id))
                    (total-payout (* income-per-share shares-owned))
                )
                    (if (> total-payout u0)
                        (match (stx-transfer? total-payout contract-owner shareholder)
                            success (begin
                                (map-set shareholder-earnings 
                                    { property-id: property-id, shareholder: shareholder }
                                    (+ (get-shareholder-earnings property-id shareholder) total-payout)
                                )
                                { property-id: property-id, income-per-share: income-per-share, result: (ok true) }
                            )
                            error { property-id: property-id, income-per-share: income-per-share, result: (err error) }
                        )
                        { property-id: property-id, income-per-share: income-per-share, result: (ok true) }
                    )
                )
                { property-id: property-id, income-per-share: income-per-share, result: (ok true) }
            )
        )
        context
    )
)

(define-read-only (get-rental-period (property-id uint) (period-start uint))
    (map-get? rental-periods { property-id: property-id, period-start: period-start })
)

(define-read-only (get-shareholder-earnings (property-id uint) (shareholder principal))
    (default-to u0 (map-get? shareholder-earnings { property-id: property-id, shareholder: shareholder }))
)

(define-read-only (get-total-distributed)
    (var-get total-distributed)
)

(define-read-only (is-contract-paused)
    (var-get contract-paused)
)

(define-read-only (get-pending-distributions (property-id uint))
    (let ((sample-periods (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)))
        (fold check-period-for-pending sample-periods 
            { property-id: property-id, pending-count: u0 }
        )
    )
)

(define-private (check-period-for-pending 
    (period-start uint) 
    (acc { property-id: uint, pending-count: uint })
)
    (let ((period-key { property-id: (get property-id acc), period-start: period-start }))
        (match (map-get? rental-periods period-key)
            period-data (if (and 
                (not (get distributed period-data))
                (>= stacks-block-height (get period-end period-data))
            )
                { 
                    property-id: (get property-id acc), 
                    pending-count: (+ (get pending-count acc) u1) 
                }
                acc
            )
            acc
        )
    )
)
