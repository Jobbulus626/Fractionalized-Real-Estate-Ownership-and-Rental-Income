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

;; ========================================
;; PROPERTY GOVERNANCE AND VOTING SYSTEM
;; ========================================

;; Additional error constants for governance
(define-constant err-proposal-active (err u404))
(define-constant err-proposal-expired (err u405))
(define-constant err-already-voted (err u406))
(define-constant err-insufficient-quorum (err u407))
(define-constant err-proposal-failed (err u408))
(define-constant err-proposal-executed (err u409))
(define-constant err-invalid-proposal-type (err u410))

;; Proposal types for different property decisions
(define-constant proposal-type-repair u1)
(define-constant proposal-type-renovation u2)
(define-constant proposal-type-rent-change u3)
(define-constant proposal-type-sell-property u4)
(define-constant proposal-type-tenant-policy u5)

;; Governance parameters
(define-constant voting-period-blocks u1440) ;; Approximately 10 days
(define-constant quorum-percentage u51) ;; 51% of shares needed for quorum
(define-constant approval-threshold u60) ;; 60% of votes needed to pass

(define-data-var next-proposal-id uint u1)

;; Proposal data structure for governance decisions
(define-map proposals
    uint
    {
        property-id: uint,
        proposer: principal,
        proposal-type: uint,
        title: (string-ascii 128),
        description: (string-ascii 512),
        proposed-value: uint, ;; For rent changes, repair costs, etc.
        votes-for: uint,
        votes-against: uint,
        total-votes: uint,
        start-block: uint,
        end-block: uint,
        executed: bool,
        passed: bool
    }
)

;; Track individual votes to prevent double voting
(define-map shareholder-votes
    { proposal-id: uint, voter: principal }
    { vote-weight: uint, voted-for: bool }
)

;; Track proposal outcomes for property history
(define-map property-governance-history
    { property-id: uint, proposal-id: uint }
    { outcome: bool, execution-block: (optional uint) }
)

;; Supporting maps for proposal execution tracking
(define-map property-repair-approvals
    { property-id: uint, approval-block: uint }
    { approved-amount: uint, completed: bool }
)

(define-map property-renovation-approvals
    { property-id: uint, approval-block: uint }
    { approved-budget: uint, started: bool }
)

(define-map property-tenant-policies
    uint
    { policy-type: uint, effective-block: uint }
)

;; Create a new governance proposal for property decisions
(define-public (create-governance-proposal
    (property-id uint)
    (proposal-type uint)
    (title (string-ascii 128))
    (description (string-ascii 512))
    (proposed-value uint)
)
    (let (
        (proposal-id (var-get next-proposal-id))
        (property (unwrap! (contract-call? .rental get-property property-id) err-not-found))
        (proposer-shares (contract-call? .rental get-shares-owned tx-sender property-id))
    )
        ;; Validate proposer owns shares in the property
        (asserts! (> proposer-shares u0) err-unauthorized)
        ;; Validate proposal type is within acceptable range
        (asserts! (and (>= proposal-type u1) (<= proposal-type u5)) err-invalid-proposal-type)
        
        (map-insert proposals proposal-id
            {
                property-id: property-id,
                proposer: tx-sender,
                proposal-type: proposal-type,
                title: title,
                description: description,
                proposed-value: proposed-value,
                votes-for: u0,
                votes-against: u0,
                total-votes: u0,
                start-block: stacks-block-height,
                end-block: (+ stacks-block-height voting-period-blocks),
                executed: false,
                passed: false
            }
        )
        
        (var-set next-proposal-id (+ proposal-id u1))
        (ok proposal-id)
    )
)

;; Cast a weighted vote on a governance proposal
(define-public (vote-on-governance-proposal
    (proposal-id uint)
    (vote-for bool)
)
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) err-not-found))
        (property-id (get property-id proposal))
        (voter-shares (contract-call? .rental get-shares-owned tx-sender property-id))
        (vote-key { proposal-id: proposal-id, voter: tx-sender })
    )
        ;; Validate voter owns shares in the property
        (asserts! (> voter-shares u0) err-unauthorized)
        ;; Check proposal is still within voting period
        (asserts! (<= stacks-block-height (get end-block proposal)) err-proposal-expired)
        ;; Prevent double voting by same shareholder
        (asserts! (is-none (map-get? shareholder-votes vote-key)) err-already-voted)
        
        ;; Record the weighted vote
        (map-insert shareholder-votes vote-key
            { vote-weight: voter-shares, voted-for: vote-for }
        )
        
        ;; Update proposal vote tallies based on share ownership
        (let (
            (new-votes-for (if vote-for 
                (+ (get votes-for proposal) voter-shares) 
                (get votes-for proposal)
            ))
            (new-votes-against (if (not vote-for) 
                (+ (get votes-against proposal) voter-shares) 
                (get votes-against proposal)
            ))
            (new-total-votes (+ (get total-votes proposal) voter-shares))
        )
            (map-set proposals proposal-id
                (merge proposal {
                    votes-for: new-votes-for,
                    votes-against: new-votes-against,
                    total-votes: new-total-votes
                })
            )
        )
        
        (ok true)
    )
)

;; Finalize a proposal after voting period ends
(define-public (finalize-governance-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) err-not-found))
        (property-id (get property-id proposal))
        (property (unwrap! (contract-call? .rental get-property property-id) err-not-found))
        (total-shares (get total-shares property))
        (required-quorum (/ (* total-shares quorum-percentage) u100))
        (required-approval (/ (* (get total-votes proposal) approval-threshold) u100))
    )
        ;; Check voting period has ended
        (asserts! (> stacks-block-height (get end-block proposal)) err-proposal-active)
        ;; Check proposal hasn't been executed already
        (asserts! (not (get executed proposal)) err-proposal-executed)
        
        ;; Verify quorum requirement is met
        (asserts! (>= (get total-votes proposal) required-quorum) err-insufficient-quorum)
        
        ;; Determine if proposal passed based on approval threshold
        (let ((proposal-passed (>= (get votes-for proposal) required-approval)))
            ;; Update proposal execution status
            (map-set proposals proposal-id
                (merge proposal { executed: true, passed: proposal-passed })
            )
            
            ;; Record governance decision in property history
            (map-insert property-governance-history
                { property-id: property-id, proposal-id: proposal-id }
                { outcome: proposal-passed, execution-block: (some stacks-block-height) }
            )
            
            ;; Execute proposal if it passed voting
            (if proposal-passed
                (begin
                    (try! (execute-governance-proposal proposal-id))
                    (ok true)
                )
                (ok false)
            )
        )
    )
)

;; Execute approved governance proposals based on their type
(define-private (execute-governance-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? proposals proposal-id) err-not-found))
        (proposal-type (get proposal-type proposal))
        (property-id (get property-id proposal))
        (proposed-value (get proposed-value proposal))
    )
        ;; Execute different proposal types with appropriate actions
        (if (is-eq proposal-type proposal-type-rent-change)
            (contract-call? .rental update-rental-price property-id proposed-value)
            (if (is-eq proposal-type proposal-type-repair)
                (execute-repair-governance-proposal property-id proposed-value)
                (if (is-eq proposal-type proposal-type-renovation)
                    (execute-renovation-governance-proposal property-id proposed-value)
                    (if (is-eq proposal-type proposal-type-tenant-policy)
                        (execute-tenant-policy-governance-proposal property-id proposed-value)
                        (ok true) ;; Default case for sell-property or future types
                    )
                )
            )
        )
    )
)

;; Execute repair proposal - tracks approved repair funds
(define-private (execute-repair-governance-proposal (property-id uint) (repair-cost uint))
    (begin
        ;; Record repair approval for property management tracking
        (map-set property-repair-approvals
            { property-id: property-id, approval-block: stacks-block-height }
            { approved-amount: repair-cost, completed: false }
        )
        (ok true)
    )
)

;; Execute renovation proposal - tracks approved renovation budget
(define-private (execute-renovation-governance-proposal (property-id uint) (renovation-budget uint))
    (begin
        ;; Record renovation approval for project management
        (map-set property-renovation-approvals
            { property-id: property-id, approval-block: stacks-block-height }
            { approved-budget: renovation-budget, started: false }
        )
        (ok true)
    )
)

;; Execute tenant policy changes based on governance decision
(define-private (execute-tenant-policy-governance-proposal (property-id uint) (policy-code uint))
    (begin
        ;; Update tenant policies with new governance-approved rules
        (map-set property-tenant-policies
            property-id
            { policy-type: policy-code, effective-block: stacks-block-height }
        )
        (ok true)
    )
)

;; Read-only functions for governance transparency and monitoring

(define-read-only (get-governance-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

(define-read-only (get-shareholder-vote (proposal-id uint) (voter principal))
    (map-get? shareholder-votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-governance-history (property-id uint) (proposal-id uint))
    (map-get? property-governance-history { property-id: property-id, proposal-id: proposal-id })
)

;; Get count of active governance proposals for a property
(define-read-only (get-active-governance-proposals (property-id uint))
    (let ((sample-proposals (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10)))
        (fold check-governance-proposal-active sample-proposals
            { property-id: property-id, active-count: u0 }
        )
    )
)

(define-private (check-governance-proposal-active
    (proposal-id uint)
    (acc { property-id: uint, active-count: uint })
)
    (match (map-get? proposals proposal-id)
        proposal (if (and
            (is-eq (get property-id proposal) (get property-id acc))
            (<= stacks-block-height (get end-block proposal))
            (not (get executed proposal))
        )
            {
                property-id: (get property-id acc),
                active-count: (+ (get active-count acc) u1)
            }
            acc
        )
        acc
    )
)

;; Get detailed status of a governance proposal
(define-read-only (get-governance-proposal-status (proposal-id uint))
    (match (map-get? proposals proposal-id)
        proposal {
            is-active: (<= stacks-block-height (get end-block proposal)),
            is-executed: (get executed proposal),
            vote-participation: (/ (* (get total-votes proposal) u100) u1), ;; Simplified calculation
            currently-passing: (> (get votes-for proposal) (get votes-against proposal))
        }
        {
            is-active: false,
            is-executed: false,
            vote-participation: u0,
            currently-passing: false
        }
    )
)

;; Check repair approval status for property management
(define-read-only (get-repair-governance-status (property-id uint) (approval-block uint))
    (map-get? property-repair-approvals { property-id: property-id, approval-block: approval-block })
)

;; Check renovation approval status for project tracking
(define-read-only (get-renovation-governance-status (property-id uint) (approval-block uint))
    (map-get? property-renovation-approvals { property-id: property-id, approval-block: approval-block })
)

;; Get current tenant policy set by governance
(define-read-only (get-tenant-governance-policy (property-id uint))
    (map-get? property-tenant-policies property-id)
)



