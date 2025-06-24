;; Custom Reminders Smart Contract
;; Manages notifications for parties, contributions, and fund lock/unlock dates

;; Error codes
(define-constant ERR_NOT_AUTHORIZED (err u401))
(define-constant ERR_REMINDER_NOT_FOUND (err u404))
(define-constant ERR_INVALID_DATE (err u400))
(define-constant ERR_ALREADY_EXISTS (err u409))

;; Data variables
(define-data-var contract-owner principal tx-sender)
(define-data-var reminder-counter uint u0)

;; Data maps
(define-map reminders
  { reminder-id: uint }
  {
    creator: principal,
    reminder-type: (string-ascii 20),
    title: (string-ascii 100),
    description: (string-ascii 500),
    target-date: uint,
    recipients: (list 50 principal),
    is-active: bool,
    created-at: uint
  }
)

(define-map user-reminders
  { user: principal }
  { reminder-ids: (list 100 uint) }
)

(define-map party-reminders
  { party-id: uint }
  { reminder-ids: (list 20 uint) }
)

(define-map fund-lock-reminders
  { fund-id: uint, reminder-type: (string-ascii 20) }
  { reminder-id: uint }
)

;; Read-only functions

(define-read-only (get-reminder (reminder-id uint))
  (map-get? reminders { reminder-id: reminder-id })
)

(define-read-only (get-user-reminders (user principal))
  (default-to 
    { reminder-ids: (list) }
    (map-get? user-reminders { user: user })
  )
)

(define-read-only (get-party-reminders (party-id uint))
  (default-to 
    { reminder-ids: (list) }
    (map-get? party-reminders { party-id: party-id })
  )
)

(define-read-only (get-fund-lock-reminder (fund-id uint) (reminder-type (string-ascii 20)))
  (map-get? fund-lock-reminders { fund-id: fund-id, reminder-type: reminder-type })
)

(define-read-only (is-reminder-active (reminder-id uint))
  (match (get-reminder reminder-id)
    reminder (and 
               (get is-active reminder)
               (> (get target-date reminder) stacks-block-height))
    false
  )
)

(define-read-only (get-active-reminders-for-user (user principal))
  (let ((user-reminder-data (get-user-reminders user)))
    (filter is-reminder-active (get reminder-ids user-reminder-data))
  )
)

;; Private functions

(define-private (add-reminder-to-user (user principal) (reminder-id uint))
  (let ((current-reminders (get-user-reminders user)))
    (match (as-max-len? (append (get reminder-ids current-reminders) reminder-id) u100)
      updated-list (begin
                     (map-set user-reminders
                       { user: user }
                       { reminder-ids: updated-list })
                     (ok true))
      (err u500))
  )
)

(define-private (add-reminder-to-party (party-id uint) (reminder-id uint))
  (let ((current-reminders (get-party-reminders party-id)))
    (match (as-max-len? (append (get reminder-ids current-reminders) reminder-id) u20)
      updated-list (begin
                     (map-set party-reminders
                       { party-id: party-id }
                       { reminder-ids: updated-list })
                     (ok true))
      (err u500))
  )
)

(define-private (add-reminder-to-recipients (recipients (list 50 principal)) (reminder-id uint))
  (begin
    (map add-reminder-to-user-simple recipients (list reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id reminder-id))
    (ok true)
  )
)

(define-private (add-reminder-to-user-simple (user principal) (reminder-id uint))
  (let ((current-reminders (get-user-reminders user)))
    (match (as-max-len? (append (get reminder-ids current-reminders) reminder-id) u100)
      updated-list (map-set user-reminders
                     { user: user }
                     { reminder-ids: updated-list })
      false)
  )
)

;; Public functions

(define-public (create-party-reminder 
  (party-id uint)
  (title (string-ascii 100))
  (description (string-ascii 500))
  (target-date uint)
  (recipients (list 50 principal)))
  (let ((reminder-id (+ (var-get reminder-counter) u1)))
    (asserts! (> target-date stacks-block-height) ERR_INVALID_DATE)
    
    ;; Create the reminder
    (map-set reminders
      { reminder-id: reminder-id }
      {
        creator: tx-sender,
        reminder-type: "party",
        title: title,
        description: description,
        target-date: target-date,
        recipients: recipients,
        is-active: true,
        created-at: stacks-block-height
      }
    )
    
    ;; Update counter
    (var-set reminder-counter reminder-id)
    
    ;; Add to party reminders
    (try! (add-reminder-to-party party-id reminder-id))
    
    ;; Add to each recipient's reminders
    (unwrap! (add-reminder-to-recipients recipients reminder-id) (err u500))
    
    (ok reminder-id)
  )
)

(define-public (create-contribution-deadline-reminder
  (party-id uint)
  (deadline-date uint)
  (recipients (list 50 principal)))
  (let ((reminder-id (+ (var-get reminder-counter) u1))
        (title "Contribution Deadline Approaching")
        (description "Don't forget to make your contribution before the deadline!"))
    (asserts! (> deadline-date stacks-block-height) ERR_INVALID_DATE)
    
    ;; Create the reminder
    (map-set reminders
      { reminder-id: reminder-id }
      {
        creator: tx-sender,
        reminder-type: "contribution",
        title: title,
        description: description,
        target-date: deadline-date,
        recipients: recipients,
        is-active: true,
        created-at: stacks-block-height
      }
    )
    
    ;; Update counter
    (var-set reminder-counter reminder-id)
    
    ;; Add to party reminders
    (try! (add-reminder-to-party party-id reminder-id))
    
    ;; Add to each recipient's reminders
    (unwrap! (add-reminder-to-recipients recipients reminder-id) (err u500))
    
    (ok reminder-id)
  )
)

(define-public (create-fund-lock-reminder
  (fund-id uint)
  (lock-date uint)
  (reminder-type (string-ascii 20))
  (recipients (list 50 principal)))
  (let ((reminder-id (+ (var-get reminder-counter) u1))
        (title (if (is-eq reminder-type "lock")
                   "Fund Lock Date Approaching"
                   "Fund Unlock Date Approaching"))
        (description (if (is-eq reminder-type "lock")
                        "Funds will be locked soon. Make sure all contributions are complete."
                        "Funds will be unlocked soon. Prepare for distribution.")))
    (asserts! (> lock-date stacks-block-height) ERR_INVALID_DATE)
    (asserts! (is-none (get-fund-lock-reminder fund-id reminder-type)) ERR_ALREADY_EXISTS)
    
    ;; Create the reminder
    (map-set reminders
      { reminder-id: reminder-id }
      {
        creator: tx-sender,
        reminder-type: reminder-type,
        title: title,
        description: description,
        target-date: lock-date,
        recipients: recipients,
        is-active: true,
        created-at: stacks-block-height
      }
    )
    
    ;; Update counter
    (var-set reminder-counter reminder-id)
    
    ;; Map fund lock reminder
    (map-set fund-lock-reminders
      { fund-id: fund-id, reminder-type: reminder-type }
      { reminder-id: reminder-id }
    )
    
    ;; Add to each recipient's reminders
    (unwrap! (add-reminder-to-recipients recipients reminder-id) (err u500))
    
    (ok reminder-id)
  )
)

(define-public (create-custom-reminder
  (title (string-ascii 100))
  (description (string-ascii 500))
  (target-date uint)
  (recipients (list 50 principal)))
  (let ((reminder-id (+ (var-get reminder-counter) u1)))
    (asserts! (> target-date stacks-block-height) ERR_INVALID_DATE)
    
    ;; Create the reminder
    (map-set reminders
      { reminder-id: reminder-id }
      {
        creator: tx-sender,
        reminder-type: "custom",
        title: title,
        description: description,
        target-date: target-date,
        recipients: recipients,
        is-active: true,
        created-at: stacks-block-height
      }
    )
    
    ;; Update counter
    (var-set reminder-counter reminder-id)
    
    ;; Add to each recipient's reminders
    (unwrap! (add-reminder-to-recipients recipients reminder-id) (err u500))

    
    (ok reminder-id)
  )
)

(define-public (deactivate-reminder (reminder-id uint))
  (let ((reminder (unwrap! (get-reminder reminder-id) ERR_REMINDER_NOT_FOUND)))
    (asserts! (or (is-eq tx-sender (get creator reminder))
                  (is-eq tx-sender (var-get contract-owner))) ERR_NOT_AUTHORIZED)
    
    (map-set reminders
      { reminder-id: reminder-id }
      (merge reminder { is-active: false })
    )
    
    (ok true)
  )
)

(define-public (update-reminder-date (reminder-id uint) (new-date uint))
  (let ((reminder (unwrap! (get-reminder reminder-id) ERR_REMINDER_NOT_FOUND)))
    (asserts! (is-eq tx-sender (get creator reminder)) ERR_NOT_AUTHORIZED)
    (asserts! (> new-date stacks-block-height) ERR_INVALID_DATE)
    
    (map-set reminders
      { reminder-id: reminder-id }
      (merge reminder { target-date: new-date })
    )
    
    (ok true)
  )
)

;; Admin functions

(define-public (set-contract-owner (new-owner principal))
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (var-set contract-owner new-owner)
    (ok true)
  )
)

;; Utility functions for frontend integration

(define-read-only (get-upcoming-reminders-for-user (user principal) (days-ahead uint))
  (let ((user-reminders-list (get-active-reminders-for-user user))
        (target-block (+ stacks-block-height (* days-ahead u144)))) ;; Assuming ~144 blocks per day
    (filter 
      is-upcoming-reminder
      user-reminders-list)
  )
)

;; Helper function for filtering upcoming reminders
(define-private (is-upcoming-reminder (reminder-id uint))
  (match (get-reminder reminder-id)
    reminder (<= (get target-date reminder) (+ stacks-block-height (* u7 u144))) ;; 7 days ahead
    false
  )
)

(define-read-only (get-reminders-within-days (user principal) (days-ahead uint))
  (let ((user-reminders-list (get-active-reminders-for-user user))
        (target-block (+ stacks-block-height (* days-ahead u144))))
    (fold check-reminder-date user-reminders-list (list))
  )
)

;; Helper function for fold operation
(define-private (check-reminder-date (reminder-id uint) (acc (list 100 uint)))
  (match (get-reminder reminder-id)
    reminder (if (<= (get target-date reminder) (+ stacks-block-height (* u7 u144)))
               (unwrap! (as-max-len? (append acc reminder-id) u100) acc)
               acc)
    acc
  )
)

(define-read-only (get-reminder-count)
  (var-get reminder-counter)
)
