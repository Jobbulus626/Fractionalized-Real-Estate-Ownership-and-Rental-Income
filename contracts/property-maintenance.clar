;; Property Maintenance Tracker Contract
;; Manages maintenance issues, budgets, and contractor assignments for fractionalized real estate

;; Error constants
(define-constant ERR-UNAUTHORIZED (err u500))
(define-constant ERR-NOT-FOUND (err u501))
(define-constant ERR-INVALID-AMOUNT (err u502))
(define-constant ERR-ALREADY-EXISTS (err u503))
(define-constant ERR-INSUFFICIENT-BUDGET (err u504))
(define-constant ERR-ISSUE-COMPLETED (err u505))
(define-constant ERR-INVALID-STATUS (err u506))
(define-constant ERR-INVALID-PRIORITY (err u507))

;; Issue status constants
(define-constant STATUS-REPORTED u1)
(define-constant STATUS-APPROVED u2)
(define-constant STATUS-IN-PROGRESS u3)
(define-constant STATUS-COMPLETED u4)
(define-constant STATUS-REJECTED u5)

;; Priority levels
(define-constant PRIORITY-LOW u1)
(define-constant PRIORITY-MEDIUM u2)
(define-constant PRIORITY-HIGH u3)
(define-constant PRIORITY-URGENT u4)

;; Data variables
(define-data-var next-issue-id uint u1)
(define-data-var contract-owner principal tx-sender)

;; Maintenance issue tracking
(define-map maintenance-issues
  uint
  {
    property-id: uint,
    reporter: principal,
    title: (string-ascii 128),
    description: (string-ascii 512),
    priority: uint,
    estimated-cost: uint,
    actual-cost: uint,
    status: uint,
    contractor: (optional principal),
    reported-at: uint,
    completed-at: (optional uint)
  }
)

;; Property maintenance budgets
(define-map property-maintenance-budgets
  uint
  {
    total-budget: uint,
    used-budget: uint,
    annual-allocation: uint,
    last-updated: uint
  }
)

;; Issue voting by shareholders
(define-map issue-votes
  { issue-id: uint, voter: principal }
  { vote-weight: uint, approved: bool }
)

;; Issue vote tallies
(define-map issue-vote-summary
  uint
  {
    total-votes-for: uint,
    total-votes-against: uint,
    voting-deadline: uint,
    finalized: bool
  }
)

;; Contractor performance tracking
(define-map contractor-performance
  principal
  {
    jobs-completed: uint,
    total-spent: uint,
    average-rating: uint,
    active-jobs: uint
  }
)

;; Property maintenance statistics
(define-map property-maintenance-stats
  uint
  {
    total-issues: uint,
    completed-issues: uint,
    total-spent: uint,
    average-resolution-time: uint,
    last-maintenance: uint
  }
)

;; Create a new maintenance issue
(define-public (report-maintenance-issue
  (property-id uint)
  (title (string-ascii 128))
  (description (string-ascii 512))
  (priority uint)
  (estimated-cost uint)
)
  (let (
    (issue-id (var-get next-issue-id))
    (reporter-shares (contract-call? .rental get-shares-owned tx-sender property-id))
    (current-stats (get-property-stats property-id))
  )
    ;; Validate reporter is a shareholder
    (asserts! (> reporter-shares u0) ERR-UNAUTHORIZED)
    ;; Validate priority level
    (asserts! (and (>= priority PRIORITY-LOW) (<= priority PRIORITY-URGENT)) ERR-INVALID-PRIORITY)
    ;; Validate estimated cost
    (asserts! (> estimated-cost u0) ERR-INVALID-AMOUNT)
    
    ;; Create the maintenance issue
    (map-set maintenance-issues issue-id
      {
        property-id: property-id,
        reporter: tx-sender,
        title: title,
        description: description,
        priority: priority,
        estimated-cost: estimated-cost,
        actual-cost: u0,
        status: STATUS-REPORTED,
        contractor: none,
        reported-at: stacks-block-height,
        completed-at: none
      }
    )
    
    ;; Initialize voting for the issue (7-day voting period)
    (map-set issue-vote-summary issue-id
      {
        total-votes-for: u0,
        total-votes-against: u0,
        voting-deadline: (+ stacks-block-height u1008), ;; ~7 days
        finalized: false
      }
    )
    
    ;; Update property statistics
    (map-set property-maintenance-stats property-id
      (merge current-stats { total-issues: (+ (get total-issues current-stats) u1) })
    )
    
    (var-set next-issue-id (+ issue-id u1))
    (ok issue-id)
  )
)

;; Vote on maintenance issue approval
(define-public (vote-on-maintenance-issue (issue-id uint) (approve bool))
  (let (
    (issue (unwrap! (map-get? maintenance-issues issue-id) ERR-NOT-FOUND))
    (property-id (get property-id issue))
    (voter-shares (contract-call? .rental get-shares-owned tx-sender property-id))
    (vote-summary (unwrap! (map-get? issue-vote-summary issue-id) ERR-NOT-FOUND))
    (vote-key { issue-id: issue-id, voter: tx-sender })
  )
    ;; Validate voter is a shareholder
    (asserts! (> voter-shares u0) ERR-UNAUTHORIZED)
    ;; Check voting is still open
    (asserts! (< stacks-block-height (get voting-deadline vote-summary)) ERR-INVALID-STATUS)
    ;; Prevent double voting
    (asserts! (is-none (map-get? issue-votes vote-key)) ERR-ALREADY-EXISTS)
    
    ;; Record the vote
    (map-set issue-votes vote-key
      { vote-weight: voter-shares, approved: approve }
    )
    
    ;; Update vote tallies
    (map-set issue-vote-summary issue-id
      (if approve
        (merge vote-summary { total-votes-for: (+ (get total-votes-for vote-summary) voter-shares) })
        (merge vote-summary { total-votes-against: (+ (get total-votes-against vote-summary) voter-shares) })
      )
    )
    
    (ok true)
  )
)

;; Finalize maintenance issue voting and approve if passed
(define-public (finalize-maintenance-voting (issue-id uint))
  (let (
    (issue (unwrap! (map-get? maintenance-issues issue-id) ERR-NOT-FOUND))
    (vote-summary (unwrap! (map-get? issue-vote-summary issue-id) ERR-NOT-FOUND))
    (property-id (get property-id issue))
    (budget-info (get-property-budget property-id))
  )
    ;; Check voting period has ended
    (asserts! (>= stacks-block-height (get voting-deadline vote-summary)) ERR-INVALID-STATUS)
    ;; Check not already finalized
    (asserts! (not (get finalized vote-summary)) ERR-ALREADY-EXISTS)
    
    ;; Determine if issue is approved (simple majority)
    (let ((issue-approved (> (get total-votes-for vote-summary) (get total-votes-against vote-summary))))
      ;; Update vote summary
      (map-set issue-vote-summary issue-id
        (merge vote-summary { finalized: true })
      )
      
      ;; Update issue status based on voting result
      (if issue-approved
        (begin
          ;; Check budget availability
          (asserts! (>= (- (get total-budget budget-info) (get used-budget budget-info)) (get estimated-cost issue)) ERR-INSUFFICIENT-BUDGET)
          ;; Approve the issue
          (map-set maintenance-issues issue-id
            (merge issue { status: STATUS-APPROVED })
          )
        )
        ;; Reject the issue
        (map-set maintenance-issues issue-id
          (merge issue { status: STATUS-REJECTED })
        )
      )
      
      (ok issue-approved)
    )
  )
)

;; Assign contractor to approved maintenance issue
(define-public (assign-contractor (issue-id uint) (contractor principal))
  (let (
    (issue (unwrap! (map-get? maintenance-issues issue-id) ERR-NOT-FOUND))
    (property-id (get property-id issue))
    (caller-shares (contract-call? .rental get-shares-owned tx-sender property-id))
  )
    ;; Only shareholders can assign contractors
    (asserts! (> caller-shares u0) ERR-UNAUTHORIZED)
    ;; Issue must be approved
    (asserts! (is-eq (get status issue) STATUS-APPROVED) ERR-INVALID-STATUS)
    
    ;; Assign contractor and update status
    (map-set maintenance-issues issue-id
      (merge issue { 
        contractor: (some contractor),
        status: STATUS-IN-PROGRESS
      })
    )
    
    ;; Update contractor's active job count
    (let ((contractor-stats (get-contractor-performance contractor)))
      (map-set contractor-performance contractor
        (merge contractor-stats { active-jobs: (+ (get active-jobs contractor-stats) u1) })
      )
    )
    
    (ok true)
  )
)

;; Complete maintenance issue and record costs
(define-public (complete-maintenance-issue (issue-id uint) (actual-cost uint) (quality-rating uint))
  (let (
    (issue (unwrap! (map-get? maintenance-issues issue-id) ERR-NOT-FOUND))
    (property-id (get property-id issue))
    (contractor (unwrap! (get contractor issue) ERR-NOT-FOUND))
    (budget-info (get-property-budget property-id))
    (property-stats (get-property-stats property-id))
    (contractor-stats (get-contractor-performance contractor))
  )
    ;; Only assigned contractor can complete
    (asserts! (is-eq tx-sender contractor) ERR-UNAUTHORIZED)
    ;; Issue must be in progress
    (asserts! (is-eq (get status issue) STATUS-IN-PROGRESS) ERR-INVALID-STATUS)
    ;; Check actual cost doesn't exceed budget
    (asserts! (<= actual-cost (get estimated-cost issue)) ERR-INSUFFICIENT-BUDGET)
    
    ;; Complete the issue
    (map-set maintenance-issues issue-id
      (merge issue { 
        actual-cost: actual-cost,
        status: STATUS-COMPLETED,
        completed-at: (some stacks-block-height)
      })
    )
    
    ;; Update property budget
    (map-set property-maintenance-budgets property-id
      (merge budget-info { used-budget: (+ (get used-budget budget-info) actual-cost) })
    )
    
    ;; Update property statistics
    (map-set property-maintenance-stats property-id
      (merge property-stats { 
        completed-issues: (+ (get completed-issues property-stats) u1),
        total-spent: (+ (get total-spent property-stats) actual-cost),
        last-maintenance: stacks-block-height
      })
    )
    
    ;; Update contractor performance
    (map-set contractor-performance contractor
      (merge contractor-stats {
        jobs-completed: (+ (get jobs-completed contractor-stats) u1),
        total-spent: (+ (get total-spent contractor-stats) actual-cost),
        active-jobs: (- (get active-jobs contractor-stats) u1),
        average-rating: quality-rating ;; Simplified - in real system would be weighted average
      })
    )
    
    (ok true)
  )
)

;; Set annual maintenance budget for a property
(define-public (set-property-maintenance-budget (property-id uint) (annual-budget uint))
  (let (
    (caller-shares (contract-call? .rental get-shares-owned tx-sender property-id))
    (current-budget (get-property-budget property-id))
  )
    ;; Only shareholders can set budgets
    (asserts! (> caller-shares u0) ERR-UNAUTHORIZED)
    (asserts! (> annual-budget u0) ERR-INVALID-AMOUNT)
    
    (map-set property-maintenance-budgets property-id
      (merge current-budget {
        total-budget: annual-budget,
        annual-allocation: annual-budget,
        last-updated: stacks-block-height
      })
    )
    
    (ok true)
  )
)

;; Read-only functions

(define-read-only (get-maintenance-issue (issue-id uint))
  (map-get? maintenance-issues issue-id)
)

(define-read-only (get-issue-vote-summary (issue-id uint))
  (map-get? issue-vote-summary issue-id)
)

(define-read-only (get-shareholder-vote (issue-id uint) (voter principal))
  (map-get? issue-votes { issue-id: issue-id, voter: voter })
)

(define-read-only (get-property-budget (property-id uint))
  (default-to
    { total-budget: u0, used-budget: u0, annual-allocation: u0, last-updated: u0 }
    (map-get? property-maintenance-budgets property-id)
  )
)

(define-read-only (get-property-stats (property-id uint))
  (default-to
    { 
      total-issues: u0, 
      completed-issues: u0, 
      total-spent: u0, 
      average-resolution-time: u0,
      last-maintenance: u0
    }
    (map-get? property-maintenance-stats property-id)
  )
)

(define-read-only (get-contractor-performance (contractor principal))
  (default-to
    { jobs-completed: u0, total-spent: u0, average-rating: u0, active-jobs: u0 }
    (map-get? contractor-performance contractor)
  )
)

(define-read-only (get-budget-availability (property-id uint))
  (let ((budget-info (get-property-budget property-id)))
    (- (get total-budget budget-info) (get used-budget budget-info))
  )
)
