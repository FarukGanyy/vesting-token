;; vesting-token.clar
;; Time-locked token vesting contract (SIP-010)
;; - Admin creates vesting schedules; contract holds tokens
;; - Beneficiaries claim vested tokens with release
;; - Admin can revoke if schedule is revocable; unvested tokens returned to admin

;; Import SIP-010 trait
(define-trait ft-trait
  (
    (transfer? (uint principal (optional (buff 34))) (response bool uint))
    (get-balance (principal) (response uint uint))
    (get-name () (response (string-ascii 32) uint))
    (get-symbol () (response (string-ascii 32) uint))
    (get-decimals () (response uint uint))
    (get-total-supply () (response uint uint))
    (burn? (uint principal) (response bool uint))
    (mint? (uint principal) (response bool uint))
  )
)

;; Constants for error codes
(define-constant ERR_NOT_ADMIN u100)
(define-constant ERR_NO_VEST u101)
(define-constant ERR_NOT_BENEFICIARY u102)
(define-constant ERR_NOT_REVOCABLE u103)
(define-constant ERR_ALREADY_REVOKED u104)
(define-constant ERR_NO_RELEASABLE u105)
(define-constant ERR_TRANSFER_FAIL u106)
(define-constant ERR_BAD_ARGS u107)
(define-constant ERR_BAD_STATE u108)

;; Validation types
(define-map valid-ids uint bool)

;; State variables
(define-data-var admin principal tx-sender)
(define-data-var vesting-counter uint u0)

;; Data maps
(define-map vestings
  { id: uint }
  {
    beneficiary: principal,
    token: principal,
    total: uint,
    released: uint,
    start: uint,
    cliff: uint,
    duration: uint,
    revocable: bool,
    revoked: bool
  })

;; Events
(define-private (emit-created (id uint) (beneficiary principal) (token principal) (total uint) (start uint) (cliff uint) (duration uint) (revocable bool))
  (print { event: "vesting-created", id: id, beneficiary: beneficiary, token: token, total: total, start: start, cliff: cliff, duration: duration, revocable: revocable }))

(define-private (emit-released (id uint) (to principal) (amount uint))
  (print { event: "vesting-released", id: id, to: to, amount: amount }))

(define-private (emit-revoked (id uint) (admin-addr principal) (unvested uint))
  (print { event: "vesting-revoked", id: id, admin: admin-addr, unvested: unvested }))

;; Helper functions
(define-read-only (is-admin (p principal)) 
  (is-eq p (var-get admin)))

(define-public (set-admin (p principal))
  (begin 
    (asserts! (is-admin tx-sender) (err ERR_NOT_ADMIN))
    (asserts! (is-eq p tx-sender) (err ERR_BAD_ARGS))
    (var-set admin p)
    (ok true)))

;; compute vested amount at current block height
(define-private (compute-vested-amount (now uint) (start uint) (cliff uint) (duration uint) (total uint))
  (if (is-eq duration u0)
      total  ;; instant vesting
      (if (< now (+ start cliff))
          u0  ;; before cliff
          (if (>= now (+ start duration))
              total  ;; fully vested
              ;; linear vesting: vested = total * (now - start) / duration
              (/ (* total (- now start)) duration)))))

;; Calculate releasable amount
(define-read-only (calc-releasable (total uint) (released uint) (start uint) (cliff uint) (duration uint))
  (let ((vested (compute-vested-amount u0 start cliff duration total)))
    (if (>= vested released)
        (- vested released)
        u0)))

;; Get releasable amount for a vesting ID
(define-read-only (releasable (id uint))
  (match (map-get? vestings { id: id }) vesting-data 
    (ok (calc-releasable
      (get total vesting-data)
      (get released vesting-data)
      (get start vesting-data)
      (get cliff vesting-data)
      (get duration vesting-data)))
    (err ERR_NO_VEST)))

;; Private functions for data validation and state mutation
(define-private (validate-create-vesting-args (total uint) (start uint) (cliff uint) (duration uint))
  (and 
    (> total u0)
    (>= duration cliff)
    (>= start burn-block-height)))

(define-private (validate-vesting-state (vesting-data {
    beneficiary: principal,
    token: principal,
    total: uint,
    released: uint,
    start: uint,
    cliff: uint,
    duration: uint,
    revocable: bool,
    revoked: bool
  }))
  (and 
    (> (get total vesting-data) u0)
    (>= (get duration vesting-data) (get cliff vesting-data))
    (>= (get start vesting-data) burn-block-height)
    (not (get revoked vesting-data))))

(define-private (create-vesting-record 
  (id uint) 
  (beneficiary principal) 
  (token-addr principal)
  (total uint) 
  (start uint) 
  (cliff uint) 
  (duration uint) 
  (revocable bool))
  (let ((vesting-data { 
          beneficiary: beneficiary,
          token: token-addr,
          total: total,
          released: u0,
          start: start,
          cliff: cliff,
          duration: duration,
          revocable: revocable,
          revoked: false }))
    (if (validate-vesting-state vesting-data) 
      (begin
        (map-set vestings { id: id } vesting-data)
        (print { event: "vesting-created", id: id, beneficiary: beneficiary, token: token-addr, total: total, start: start, cliff: cliff, duration: duration, revocable: revocable })
        (ok true))
      (err ERR_BAD_ARGS))))

;; Data validation
(define-private (validate-id (id uint))
  (match (map-get? valid-ids id)
    is-valid (and is-valid true)
    false))

;; Private helper for state update
(define-private (update-vesting 
  (id uint) 
  (vesting-data {
    beneficiary: principal,
    token: principal,
    total: uint,
    released: uint,
    start: uint,
    cliff: uint,
    duration: uint,
    revocable: bool,
    revoked: bool
  }))
  (begin
    (asserts! (validate-id id) (err ERR_BAD_STATE))
    (map-set vestings { id: id } vesting-data)
    (ok true)))

;; Create vesting schedule
(define-public (create-vesting
  (beneficiary principal)
  (token <ft-trait>)
  (total uint)
  (start uint)
  (cliff uint)
  (duration uint)
  (revocable bool))
  (begin
    ;; Validate permissions and arguments
    (asserts! (is-admin tx-sender) (err ERR_NOT_ADMIN))
    (asserts! (validate-create-vesting-args total start cliff duration) (err ERR_BAD_ARGS))
    (asserts! (> total u0) (err ERR_BAD_ARGS))
    
    (let ((id (+ (var-get vesting-counter) u1))
          (token-addr (contract-of token))
          (balance-response (unwrap! (contract-call? token get-balance tx-sender) (err ERR_BAD_ARGS))))
      (asserts! (>= balance-response total) (err ERR_BAD_ARGS))
      
      ;; Create and validate record
      (let ((vesting-data {
              beneficiary: beneficiary,
              token: token-addr,
              total: total,
              released: u0,
              start: start,
              cliff: cliff,
              duration: duration,
              revocable: revocable,
              revoked: false }))
        ;; Update state
        (map-set valid-ids id true)
        (try! (update-vesting id vesting-data))
        (var-set vesting-counter id)
        (print { 
          event: "vesting-created", 
          id: id, 
          beneficiary: beneficiary, 
          token: token-addr, 
          total: total, 
          start: start, 
          cliff: cliff, 
          duration: duration, 
          revocable: revocable 
        })
        (ok id)))))

;; Release vested tokens 
(define-public (release (id uint) (token-trait <ft-trait>))
  (begin
    ;; Validate ID
    (asserts! (validate-id id) (err ERR_BAD_STATE))
    
    ;; Load and validate vesting data
    (match (map-get? vestings { id: id }) vesting-data
      (let ((benef (get beneficiary vesting-data))
            (total (get total vesting-data))
            (released (get released vesting-data))
            (start (get start vesting-data))
            (cliff (get cliff vesting-data))
            (duration (get duration vesting-data))
            (releasable (calc-releasable total released start cliff duration))
            (token-contract (get token vesting-data)))
        (asserts! (is-eq tx-sender benef) (err ERR_NOT_BENEFICIARY))
        (asserts! (> releasable u0) (err ERR_NO_RELEASABLE))
        (asserts! (is-eq (contract-of token-trait) token-contract) (err ERR_BAD_ARGS))
        
        ;; Execute transfer and update state
        (match (as-contract (contract-call? token-trait transfer? releasable benef none))
          success-result (begin
            (try! (update-vesting id (merge vesting-data { released: (+ released releasable) })))
            (print { event: "vesting-released", id: id, to: benef, amount: releasable })
            (ok releasable))
          err-code (err ERR_TRANSFER_FAIL)))
      (err ERR_NO_VEST))))

;; Revoke vesting (admin only, if revocable)
(define-public (revoke (id uint) (token-trait <ft-trait>))
  (begin
    ;; Validate admin access and ID
    (asserts! (is-admin tx-sender) (err ERR_NOT_ADMIN))
    (asserts! (validate-id id) (err ERR_BAD_STATE))
    
    ;; Load and validate vesting data
    (match (map-get? vestings { id: id }) vesting-data
      (let ((total (get total vesting-data))
            (start (get start vesting-data))
            (cliff (get cliff vesting-data))
            (duration (get duration vesting-data))
            (token-contract (get token vesting-data)))
        ;; Validate conditions
        (asserts! (is-eq (contract-of token-trait) token-contract) (err ERR_BAD_ARGS))
        (asserts! (get revocable vesting-data) (err ERR_NOT_REVOCABLE))
        (asserts! (not (get revoked vesting-data)) (err ERR_ALREADY_REVOKED))
        
        ;; Calculate amounts
        (let ((vested (compute-vested-amount u0 start cliff duration total))
              (unvested (if (>= total vested) (- total vested) u0)))
          
          ;; Update state
          (try! (update-vesting id (merge vesting-data { total: vested, revoked: true })))
          
          ;; Handle token transfer and return result
          (if (> unvested u0)
              (match (as-contract (contract-call? token-trait transfer? unvested (var-get admin) none))
                success-result 
                  (begin
                    (print { event: "vesting-revoked", id: id, admin: (var-get admin), unvested: unvested })
                    (ok { vested: vested, reclaimed: unvested }))
                err-code (err ERR_TRANSFER_FAIL))
              (begin
                (print { event: "vesting-revoked", id: id, admin: (var-get admin), unvested: u0 })
                (ok { vested: vested, reclaimed: u0 })))))
      (err ERR_NO_VEST))))

;; Read-only: get vesting info
(define-read-only (get-vesting (id uint))
  (map-get? vestings { id: id }))

(define-read-only (vesting-count)
  (var-get vesting-counter))