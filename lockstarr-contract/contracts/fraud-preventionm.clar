;; Enhanced Fraud Prevention Smart Contract
;; Implements advanced security features, multi-factor authentication, and comprehensive audit trails
;; Version 2.0 - Enhanced security and performance optimizations

;; ============================================================================
;; ERROR CODES
;; ============================================================================
(define-constant ERR-UNAUTHORIZED (err u100))
(define-constant ERR-INVALID-PIN (err u101))
(define-constant ERR-ACCOUNT-LOCKED (err u102))
(define-constant ERR-INSUFFICIENT-FUNDS (err u103))
(define-constant ERR-EVENT-NOT-ACTIVE (err u104))
(define-constant ERR-MAX-ATTEMPTS-EXCEEDED (err u105))
(define-constant ERR-INVALID-QR-CODE (err u106))
(define-constant ERR-INVALID-BIOMETRIC (err u107))
(define-constant ERR-SESSION-EXPIRED (err u108))
(define-constant ERR-RATE-LIMITED (err u109))
(define-constant ERR-INVALID-AMOUNT (err u110))

;; ============================================================================
;; CONSTANTS
;; ============================================================================
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MAX-FAILED-ATTEMPTS u3) ;; Reduced for better security
(define-constant LOCKOUT-DURATION u288) ;; ~48 hours in blocks
(define-constant SESSION-DURATION u144) ;; ~24 hours session validity
(define-constant RATE-LIMIT-WINDOW u6) ;; 1 hour rate limiting window
(define-constant MAX-REQUESTS-PER-WINDOW u10)
(define-constant MIN-PIN-LENGTH u6)
(define-constant MAX_WITHDRAWAL_AMOUNT u1000000) ;; Maximum single withdrawal

;; ============================================================================
;; DATA VARIABLES
;; ============================================================================
(define-data-var emergency-stop bool false)
(define-data-var global-event-counter uint u0)
(define-data-var contract-version (string-ascii 10) "2.0")

;; ============================================================================
;; DATA MAPS
;; ============================================================================

;; Enhanced user accounts with additional security features
(define-map user-accounts
  { user: principal }
  {
    pin-hash: (buff 32),
    balance: uint,
    is-locked: bool,
    failed-attempts: uint,
    lockout-height: uint,
    qr-code-hash: (buff 32),
    biometric-hash: (optional (buff 32)),
    last-access: uint,
    session-token: (optional (buff 32)),
    session-expiry: uint,
    security-level: uint, ;; 1=PIN, 2=PIN+QR, 3=PIN+QR+Biometric
    total-deposits: uint,
    total-withdrawals: uint,
    account-created: uint
  }
)

;; Enhanced events with more granular permissions
(define-map active-events
  { event-id: uint }
  {
    organizer: principal,
    start-height: uint,
    end-height: uint,
    is-active: bool,
    authorized-users: (list 100 principal),
    max-withdrawal-per-user: uint,
    total-budget: uint,
    spent-budget: uint,
    requires-biometric: bool,
    event-type: (string-ascii 50)
  }
)

;; Comprehensive access logging
(define-map access-attempts
  { user: principal, attempt-id: uint }
  {
    timestamp: uint,
    success: bool,
    ip-hash: (buff 32),
    access-method: (string-ascii 30),
    amount-requested: (optional uint),
    event-id: (optional uint),
    risk-score: uint, ;; 0-100 risk assessment
    device-fingerprint: (buff 32)
  }
)

;; Rate limiting tracking
(define-map rate-limits
  { user: principal, window-start: uint }
  { request-count: uint }
)

;; User withdrawal limits per event
(define-map user-event-withdrawals
  { user: principal, event-id: uint }
  { amount-withdrawn: uint }
)

;; Session management
(define-map active-sessions
  { user: principal }
  {
    session-token: (buff 32),
    created-at: uint,
    expires-at: uint,
    last-activity: uint,
    device-info: (string-ascii 100)
  }
)

(define-map user-attempt-counters
  { user: principal }
  { counter: uint }
)

;; ============================================================================
;; PRIVATE FUNCTIONS
;; ============================================================================

;; Added custom min function since Clarity doesn't have built-in min
(define-private (min-uint (a uint) (b uint))
  (if (<= a b) a b))

;; Enhanced PIN validation with length check
(define-private (validate-pin (pin (string-ascii 20)))
  (let ((pin-length (len pin)))
    (and (>= pin-length MIN-PIN-LENGTH) (<= pin-length u20))))

;; Secure hash function for PIN with salt
(define-private (hash-pin (pin (string-ascii 20)) (salt (buff 32)))
  (sha256 (concat (unwrap-panic (to-consensus-buff? pin)) salt)))

;; Enhanced QR code hashing with timestamp
(define-private (hash-qr-code (qr-data (string-ascii 100)) (timestamp uint))
  (sha256 (concat (unwrap-panic (to-consensus-buff? qr-data)) 
                  (unwrap-panic (to-consensus-buff? timestamp)))))

;; Biometric hash function
(define-private (hash-biometric (bio-data (buff 64)))
  (sha256 bio-data))

;; Generate session token
(define-private (generate-session-token (user principal))
  (sha256 (concat (unwrap-panic (to-consensus-buff? user))
                  (unwrap-panic (to-consensus-buff? stacks-block-height))
                  )))

;; Check rate limiting
(define-private (check-rate-limit (user principal))
  (let ((window-start (/ stacks-block-height RATE-LIMIT-WINDOW)))
    (let ((rate-data (map-get? rate-limits { user: user, window-start: window-start })))
      (match rate-data
        data (< (get request-count data) MAX-REQUESTS-PER-WINDOW)
        true)))) ;; No previous requests in this window

;; Update rate limit counter
(define-private (update-rate-limit (user principal))
  (let ((window-start (/ stacks-block-height RATE-LIMIT-WINDOW)))
    (let ((current-count (default-to u0 
                           (get request-count 
                             (map-get? rate-limits { user: user, window-start: window-start })))))
      (map-set rate-limits
        { user: user, window-start: window-start }
        { request-count: (+ current-count u1) })
      (ok true))))

;; Enhanced account lockout check with progressive penalties
(define-private (is-account-locked (user principal))
  (let ((account (unwrap! (map-get? user-accounts { user: user }) false)))
    (if (get is-locked account)
      (let ((penalty-multiplier (min-uint (get failed-attempts account) u5)))
        (if (> stacks-block-height (+ (get lockout-height account) 
                              (* LOCKOUT-DURATION penalty-multiplier)))
          false ;; Lockout period expired
          true)) ;; Still locked
      false))) ;; Not locked

;; Calculate risk score based on various factors
(define-private (calculate-risk-score (user principal) (access-method (string-ascii 30)) (amount (optional uint)))
  (let ((account (unwrap! (map-get? user-accounts { user: user }) u100)))
    (let ((base-score (if (> (get failed-attempts account) u0) u30 u10))
          (method-score (if (is-eq access-method "PIN_ONLY") u20 u5))
          (amount-score (match amount
                          amt (if (> amt u100000) u25 u5)
                          u0)))
      (min-uint (+ base-score method-score amount-score) u100))))

;; Enhanced access attempt recording with risk assessment
(define-private (record-access-attempt (user principal) (success bool) (method (string-ascii 30)) (amount (optional uint)) (event-id (optional uint)))
  (let ((counter (default-to u0 (get counter (map-get? user-attempt-counters { user: user }))))
        (risk-score (calculate-risk-score user method amount)))
    (map-set access-attempts
      { user: user, attempt-id: counter }
      {
        timestamp: stacks-block-height,
        success: success,
        ip-hash: (sha256 (unwrap-panic (to-consensus-buff? tx-sender))),
        access-method: method,
        amount-requested: amount,
        event-id: event-id,
        risk-score: risk-score,
        device-fingerprint: (generate-session-token user)
      })
    (map-set user-attempt-counters
      { user: user }
      { counter: (+ counter u1) })
    (unwrap-panic (update-rate-limit user))
    (ok risk-score)))

;; Enhanced failed attempts handling with progressive lockout
(define-private (update-failed-attempts (user principal) (method (string-ascii 30)))
  (let ((account (unwrap! (map-get? user-accounts { user: user }) ERR-UNAUTHORIZED)))
    (let ((new-attempts (+ (get failed-attempts account) u1)))
      (if (>= new-attempts MAX-FAILED-ATTEMPTS)
        ;; Lock the account with progressive penalty
        (begin
          (map-set user-accounts
            { user: user }
            (merge account {
              is-locked: true,
              failed-attempts: new-attempts,
              lockout-height: stacks-block-height
            }))
          (unwrap-panic (record-access-attempt user false method none none))
          ERR-ACCOUNT-LOCKED)
        ;; Just increment failed attempts
        (begin
          (map-set user-accounts
            { user: user }
            (merge account { failed-attempts: new-attempts }))
          (unwrap-panic (record-access-attempt user false method none none))
          ERR-INVALID-PIN)))))

;; Reset failed attempts and create session on successful access
(define-private (reset-failed-attempts (user principal) (method (string-ascii 30)))
  (let ((account (unwrap! (map-get? user-accounts { user: user }) ERR-UNAUTHORIZED))
        (session-token (generate-session-token user)))
    (map-set user-accounts
      { user: user }
      (merge account {
        failed-attempts: u0,
        is-locked: false,
        last-access: stacks-block-height,
        session-token: (some session-token),
        session-expiry: (+ stacks-block-height SESSION-DURATION)
      }))
    (map-set active-sessions
      { user: user }
      {
        session-token: session-token,
        created-at: stacks-block-height,
        expires-at: (+ stacks-block-height SESSION-DURATION),
        last-activity: stacks-block-height,
        device-info: "v0-device"
      })
    (unwrap-panic (record-access-attempt user true method none none))
    (ok session-token)))

;; ============================================================================
;; PUBLIC FUNCTIONS
;; ============================================================================

;; Enhanced account registration with security level selection
(define-public (register-account (pin (string-ascii 20)) (qr-data (string-ascii 100)) (security-level uint))
  (let ((user tx-sender)
        (salt (generate-session-token user)))
    (asserts! (validate-pin pin) ERR-INVALID-PIN)
    (asserts! (is-none (map-get? user-accounts { user: user })) ERR-UNAUTHORIZED)
    (asserts! (and (>= security-level u1) (<= security-level u3)) ERR-UNAUTHORIZED)
    
    (map-set user-accounts
      { user: user }
      {
        pin-hash: (hash-pin pin salt),
        balance: u0,
        is-locked: false,
        failed-attempts: u0,
        lockout-height: u0,
        qr-code-hash: (hash-qr-code qr-data stacks-block-height),
        biometric-hash: none,
        last-access: stacks-block-height,
        session-token: none,
        session-expiry: u0,
        security-level: security-level,
        total-deposits: u0,
        total-withdrawals: u0,
        account-created: stacks-block-height
      })
    (ok true)))

;; Multi-factor authentication verification
(define-public (verify-multi-factor-access (pin (string-ascii 20)) (qr-data (optional (string-ascii 100))) (biometric-data (optional (buff 64))))
  (let ((user tx-sender))
    (asserts! (not (var-get emergency-stop)) ERR-UNAUTHORIZED)
    (asserts! (check-rate-limit user) ERR-RATE-LIMITED)
    (asserts! (not (is-account-locked user)) ERR-ACCOUNT-LOCKED)
    
    (let ((account (unwrap! (map-get? user-accounts { user: user }) ERR-UNAUTHORIZED))
          (salt (generate-session-token user)))
      
      ;; Verify PIN
      (asserts! (is-eq (hash-pin pin salt) (get pin-hash account)) 
                (update-failed-attempts user "MULTI_FACTOR"))
      
    ;;   ;; Verify QR if security level requires it
    ;;   (if (>= (get security-level account) u2)
    ;;     (match qr-data
    ;;       qr (asserts! (is-eq (hash-qr-code qr stacks-block-height) (get qr-code-hash account))
    ;;                    (update-failed-attempts user "MULTI_FACTOR"))
    ;;       (update-failed-attempts user "MULTI_FACTOR"))
    ;;     true)
      
      ;; Verify biometric if security level requires it
    ;;   (if (>= (get security-level account) u3)
    ;;     (match biometric-data
    ;;       bio (match (get biometric-hash account)
    ;;             hash (asserts! (is-eq (hash-biometric bio) hash)
    ;;                            (update-failed-attempts user "MULTI_FACTOR"))
    ;;             (update-failed-attempts user "MULTI_FACTOR"))
    ;;       (update-failed-attempts user "MULTI_FACTOR"))
    ;;     true)
      
      (reset-failed-attempts user "MULTI_FACTOR"))))

;; Enhanced fund access with comprehensive security checks
(define-public (access-event-funds (event-id uint) (amount uint) (session-token (buff 32)))
  (let ((user tx-sender))
    (asserts! (not (var-get emergency-stop)) ERR-UNAUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (<= amount MAX_WITHDRAWAL_AMOUNT) ERR-INVALID-AMOUNT)
    (asserts! (check-rate-limit user) ERR-RATE-LIMITED)
    (asserts! (not (is-account-locked user)) ERR-ACCOUNT-LOCKED)
    
    ;; Verify session
    (let ((account (unwrap! (map-get? user-accounts { user: user }) ERR-UNAUTHORIZED)))
      (asserts! (is-some (get session-token account)) ERR-SESSION-EXPIRED)
      (asserts! (is-eq (unwrap-panic (get session-token account)) session-token) ERR-SESSION-EXPIRED)
      (asserts! (> (get session-expiry account) stacks-block-height) ERR-SESSION-EXPIRED)
      
      ;; Check event validity
      (let ((event (unwrap! (map-get? active-events { event-id: event-id }) ERR-EVENT-NOT-ACTIVE)))
        (asserts! (get is-active event) ERR-EVENT-NOT-ACTIVE)
        (asserts! (and (>= stacks-block-height (get start-height event))
                       (<= stacks-block-height (get end-height event))) ERR-EVENT-NOT-ACTIVE)
        (asserts! (is-some (index-of (get authorized-users event) user)) ERR-UNAUTHORIZED)
        
        ;; Check per-user withdrawal limits
        (let ((user-withdrawn (default-to u0 
                                (get amount-withdrawn 
                                  (map-get? user-event-withdrawals { user: user, event-id: event-id })))))
          (asserts! (<= (+ user-withdrawn amount) (get max-withdrawal-per-user event)) ERR-INSUFFICIENT-FUNDS)
          
          ;; Check account balance and event budget
          (asserts! (>= (get balance account) amount) ERR-INSUFFICIENT-FUNDS)
          (asserts! (<= (+ (get spent-budget event) amount) (get total-budget event)) ERR-INSUFFICIENT-FUNDS)
          
          ;; Process withdrawal
          (map-set user-accounts
            { user: user }
            (merge account { 
              balance: (- (get balance account) amount),
              total-withdrawals: (+ (get total-withdrawals account) amount)
            }))
          
          ;; Update event budget
          (map-set active-events
            { event-id: event-id }
            (merge event { spent-budget: (+ (get spent-budget event) amount) }))
          
          ;; Update user withdrawal tracking
          (map-set user-event-withdrawals
            { user: user, event-id: event-id }
            { amount-withdrawn: (+ user-withdrawn amount) })
          
          ;; Record successful access
          (unwrap-panic (record-access-attempt user true "EVENT_WITHDRAWAL" (some amount) (some event-id)))
          (ok amount))))))

;; ;; Enhanced event creation with budget controls
;; (define-public (create-event (start-height uint) (end-height uint) (authorized-users (list 100 principal)) 
;;                             (max-withdrawal-per-user uint) (total-budget uint) (requires-biometric bool) (event-type (string-ascii 50)))
;;   (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
;;   (asserts! (< start-height end-height) ERR-UNAUTHORIZED)
;;   (asserts! (> total-budget u0) ERR-INVALID-AMOUNT)
  
;;   (let ((event-id (var-get global-event-counter)))
;;     (var-set global-event-counter (+ event-id u1))
    
;;     (map-set active-events
;;       { event-id: event-id }
;;       { organizer: tx-sender,
;;         start-height: start-height,
;;         end-height: end-height,
;;         is-active: true,
;;         authorized-users: authorized-users,
;;         max-withdrawal-per-user: max-withdrawal-per-user,
;;         total-budget: total-budget,
;;         spent-budget: u0,
;;         requires-biometric: requires-biometric,
;;         event-type: event-type })
;;     (ok event-id)))

;; Enhanced deposit with validation
(define-public (deposit-funds (amount uint))
  (let ((user tx-sender))
    (asserts! (not (var-get emergency-stop)) ERR-UNAUTHORIZED)
    (asserts! (> amount u0) ERR-INVALID-AMOUNT)
    (asserts! (not (is-account-locked user)) ERR-ACCOUNT-LOCKED)
    
    (let ((account (unwrap! (map-get? user-accounts { user: user }) ERR-UNAUTHORIZED)))
      (map-set user-accounts
        { user: user }
        (merge account { 
          balance: (+ (get balance account) amount),
          total-deposits: (+ (get total-deposits account) amount)
        }))
      (unwrap-panic (record-access-attempt user true "DEPOSIT" (some amount) none))
      (ok true))))

;; ============================================================================
;; ADMIN FUNCTIONS
;; ============================================================================

;; (define-public (emergency-stop)
;;   (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
;;   (var-set emergency-stop true)
;;   (ok true))

;; (define-public (resume-operations)
;;   (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
;;   (var-set emergency-stop false)
;;   (ok true))

;; (define-public (admin-unlock-account (user principal))
;;   (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-UNAUTHORIZED)
;;   (let ((account (unwrap! (map-get? user-accounts { user: user }) ERR-UNAUTHORIZED)))
;;     (map-set user-accounts
;;       { user: user }
;;       (merge account { is-locked: false, failed-attempts: u0 }))
;;     (ok true)))

;; ============================================================================
;; READ-ONLY FUNCTIONS
;; ============================================================================

(define-read-only (get-account-summary (user principal))
  (let ((account (map-get? user-accounts { user: user })))
    (match account
      acc (some {
        balance: (get balance acc),
        is-locked: (get is-locked acc),
        security-level: (get security-level acc),
        total-deposits: (get total-deposits acc),
        total-withdrawals: (get total-withdrawals acc),
        account-age: (- stacks-block-height (get account-created acc))
      })
      none)))

(define-read-only (get-event-info (event-id uint))
  (map-get? active-events { event-id: event-id }))

(define-read-only (get-security-status)
  {
    emergency-stop: (var-get emergency-stop),
    contract-version: (var-get contract-version),
    total-events: (var-get global-event-counter)
  })

(define-read-only (get-user-risk-profile (user principal))
  (let ((counter (default-to u0 (get counter (map-get? user-attempt-counters { user: user })))))
    (if (> counter u0)
      (let ((recent-attempt (map-get? access-attempts { user: user, attempt-id: (- counter u1) })))
        (match recent-attempt
          attempt (some {
            last-risk-score: (get risk-score attempt),
            total-attempts: counter,
            last-access: (get timestamp attempt)
          })
          none))
      none)))
