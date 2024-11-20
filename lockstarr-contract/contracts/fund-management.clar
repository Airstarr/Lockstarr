;; Fund management contract
;; description: This contract facilitates secure fund management, including locking funds with optional partial unlocks, creating and managing group savings with contribution tracking and goal-based spending, and handling emergency unlock requests with a voting-based approval system.
;; code by Airstarr


(define-map locked-funds 
  { 
    user: principal,
    lock-id: uint
  }
  {
    amount: uint,
    unlock-date: uint,
    partial-unlock-allowed: bool
  }
)

(define-map savings-groups 
  { 
    group-id: uint 
  }
  {
    name: (string-utf8 50),
    members: (list 10 principal),
    goal-amount: uint,
    current-amount: uint,
    is-completed: bool
  }
)

(define-map emergency-unlock-requests
  {
    request-id: uint
  }
  {
    user: principal,
    amount: uint,
    reason: (string-utf8 200),
    proof-url: (optional (string-utf8 200)),
    approved-votes: uint,
    total-votes: uint,
    is-approved: bool
  }
)

;; Counter variables
(define-data-var next-lock-id uint u0)
(define-data-var next-group-id uint u0)
(define-data-var next-emergency-request-id uint u0)

;; Fund Locking Functions
(define-public (lock-funds (amount uint) (unlock-date uint) (partial-unlock bool))
  (let 
    (
      (lock-id (var-get next-lock-id))
      (sender tx-sender)
    )
    (try! (stx-transfer? amount sender (as-contract tx-sender)))
    (map-set locked-funds 
      { user: sender, lock-id: lock-id }
      { 
        amount: amount, 
        unlock-date: unlock-date,
        partial-unlock-allowed: partial-unlock
      }
    )
    (var-set next-lock-id (+ lock-id u1))
    (ok lock-id)
  )
)

(define-public (unlock-funds (lock-id uint))
  (let 
    (
      (sender tx-sender)
      (fund-details (unwrap! 
        (map-get? locked-funds { user: sender, lock-id: lock-id }) 
        (err u1001)
      ))
    )
    (asserts! (>= stacks-block-height (get unlock-date fund-details)) (err u1002))
    
    (if (get partial-unlock-allowed fund-details)
      (begin
        ;; Logic for partial unlock would go here
        (ok true)
      )
      (begin
        (try! (as-contract (stx-transfer? (get amount fund-details) tx-sender sender)))
        (map-delete locked-funds { user: sender, lock-id: lock-id })
        (ok true)
      )
    )
  )
)

;; Savings Group Functions
(define-public (create-savings-group 
  (group-name (string-utf8 50)) 
  (members (list 10 principal)) 
  (goal-amount uint)
)
  (let 
    (
      (group-id (var-get next-group-id))
    )
    (map-set savings-groups 
      { group-id: group-id }
      {
        name: group-name,
        members: members,
        goal-amount: goal-amount,
        current-amount: u0,
        is-completed: false
      }
    )
    (var-set next-group-id (+ group-id u1))
    (ok group-id)
  )
)

(define-public (contribute-to-group (group-id uint) (amount uint))
  (let 
    (
      (group-details (unwrap! 
        (map-get? savings-groups { group-id: group-id }) 
        (err u2001)
      ))
      (sender tx-sender)
    )
    (asserts! 
      (is-some (index-of (get members group-details) sender)) 
      (err u2002)
    )
    (asserts! (not (get is-completed group-details)) (err u2003))
    
    (try! (stx-transfer? amount sender (as-contract tx-sender)))
    
    (map-set savings-groups 
      { group-id: group-id }
      (merge group-details 
        { 
          current-amount: (+ (get current-amount group-details) amount)
        }
      )
    )
    (ok true)
  )
)

(define-public (approve-withdrawal (group-id uint))
  (let 
    (
      (group-details (unwrap! 
        (map-get? savings-groups { group-id: group-id }) 
        (err u3001)
      ))
    )
    ;; Multi-signature withdrawal logic would be implemented here
    (ok true)
  )
)

(define-public (finalize-goal (group-id uint))
  (let 
    (
      (group-details (unwrap! 
        (map-get? savings-groups { group-id: group-id }) 
        (err u4001)
      ))
    )
    (asserts! 
      (>= (get current-amount group-details) (get goal-amount group-details)) 
      (err u4002)
    )
    
    (map-set savings-groups 
      { group-id: group-id }
      (merge group-details { is-completed: true })
    )
    (ok true)
  )
)

;; Emergency Unlock Functions
(define-public (request-emergency-unlock 
  (reason (string-utf8 200)) 
  (proof-url (optional (string-utf8 200)))
)
  (let 
    (
      (request-id (var-get next-emergency-request-id))
      (sender tx-sender)
    )
    (map-set emergency-unlock-requests 
      { request-id: request-id }
      {
        user: sender,
        amount: u0, ;; Amount would be set dynamically
        reason: reason,
        proof-url: proof-url,
        approved-votes: u0,
        total-votes: u0,
        is-approved: false
      }
    )
    (var-set next-emergency-request-id (+ request-id u1))
    (ok request-id)
  )
)

(define-public (approve-emergency-unlock (request-id uint))
  (let 
    (
      (request-details (unwrap! 
        (map-get? emergency-unlock-requests { request-id: request-id }) 
        (err u5001)
      ))
    )
    ;; Voting and approval logic would be implemented here
    (ok true)
  )
)