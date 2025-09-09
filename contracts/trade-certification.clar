;;
;; title: trade-certification
;; version: 1.0
;; summary: Vocational Training Certification system for skilled trades
;; description: A comprehensive system for managing trade apprenticeships, competency testing, certifications, and employer recognition

;; Error constants
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_NOT_FOUND (err u101))
(define-constant ERR_ALREADY_EXISTS (err u102))
(define-constant ERR_INVALID_INPUT (err u103))
(define-constant ERR_NOT_ACTIVE (err u104))
(define-constant ERR_ALREADY_ACTIVE (err u105))

;; Contract owner
(define-data-var contract-owner principal tx-sender)
(define-data-var min-apprenticeship-hours uint u1000)

;; Registries
(define-map approved-providers principal bool)
(define-map approved-employers principal bool)

;; Core data structures
(define-map apprenticeships
  { apprentice: principal, provider: principal, trade: (string-ascii 64) }
  { start-bn: uint, end-bn: (optional uint), hours: uint, active: bool })

(define-map tests
  { apprentice: principal, trade: (string-ascii 64), test-id: uint }
  { score: uint, passed: bool, assessor: principal, taken-bn: uint })

(define-map certifications
  { apprentice: principal, trade: (string-ascii 64) }
  { level: uint, issued-by: principal, issued-at: uint, valid-until: (optional uint) })

(define-map ce-credits
  { apprentice: principal, trade: (string-ascii 64) }
  { credits: uint, last-updated: uint })

(define-map employer-recognitions
  { apprentice: principal, employer: principal, trade: (string-ascii 64) }
  { recognized: bool, at-bn: uint })

;; Governance functions
(define-private (assert-owner (caller principal))
  (if (is-eq caller (var-get contract-owner))
      (ok true)
      ERR_UNAUTHORIZED))

(define-public (set-owner (new-owner principal))
  (begin
    (try! (assert-owner tx-sender))
    (ok (var-set contract-owner new-owner))))

(define-public (set-min-hours (new-min uint))
  (begin
    (try! (assert-owner tx-sender))
    (if (> new-min u0)
        (begin (var-set min-apprenticeship-hours new-min) (ok new-min))
        ERR_INVALID_INPUT)))

;; Provider management
(define-public (register-provider (provider principal))
  (begin
    (try! (assert-owner tx-sender))
    (asserts! (is-none (map-get? approved-providers provider)) ERR_ALREADY_EXISTS)
    (map-set approved-providers provider true)
    (ok true)))

(define-public (revoke-provider (provider principal))
  (begin
    (try! (assert-owner tx-sender))
    (asserts! (is-some (map-get? approved-providers provider)) ERR_NOT_FOUND)
    (map-delete approved-providers provider)
    (ok true)))

;; Employer management
(define-public (register-employer (employer principal))
  (begin
    (try! (assert-owner tx-sender))
    (asserts! (is-none (map-get? approved-employers employer)) ERR_ALREADY_EXISTS)
    (map-set approved-employers employer true)
    (ok true)))

(define-public (revoke-employer (employer principal))
  (begin
    (try! (assert-owner tx-sender))
    (asserts! (is-some (map-get? approved-employers employer)) ERR_NOT_FOUND)
    (map-delete approved-employers employer)
    (ok true)))

;; Apprenticeship management
(define-public (start-apprenticeship (apprentice principal) (trade (string-ascii 64)))
  (let ((key { apprentice: apprentice, provider: tx-sender, trade: trade }))
    (asserts! (is-some (map-get? approved-providers tx-sender)) ERR_UNAUTHORIZED)
    (asserts! (is-none (map-get? apprenticeships key)) ERR_ALREADY_EXISTS)
    (map-set apprenticeships key
      { start-bn: stacks-block-height, end-bn: none, hours: u0, active: true })
    (ok true)))

(define-public (update-apprenticeship-hours (apprentice principal) (trade (string-ascii 64)) (additional-hours uint))
  (let ((key { apprentice: apprentice, provider: tx-sender, trade: trade }))
    (asserts! (is-some (map-get? approved-providers tx-sender)) ERR_UNAUTHORIZED)
    (match (map-get? apprenticeships key)
      apprenticeship-data
        (if (get active apprenticeship-data)
            (let ((new-hours (+ (get hours apprenticeship-data) additional-hours)))
              (map-set apprenticeships key
                (merge apprenticeship-data { hours: new-hours }))
              (ok new-hours))
            ERR_NOT_ACTIVE)
      ERR_NOT_FOUND)))

(define-public (complete-apprenticeship (apprentice principal) (trade (string-ascii 64)))
  (let ((key { apprentice: apprentice, provider: tx-sender, trade: trade }))
    (asserts! (is-some (map-get? approved-providers tx-sender)) ERR_UNAUTHORIZED)
    (match (map-get? apprenticeships key)
      apprenticeship-data
        (if (get active apprenticeship-data)
            (begin
              (map-set apprenticeships key
                (merge apprenticeship-data { end-bn: (some stacks-block-height), active: false }))
              (ok true))
            ERR_NOT_ACTIVE)
      ERR_NOT_FOUND)))

;; Testing functions
(define-public (record-test (apprentice principal) (trade (string-ascii 64)) (test-id uint) (score uint) (passed bool))
  (begin
    (asserts! (is-some (map-get? approved-providers tx-sender)) ERR_UNAUTHORIZED)
    (let ((key { apprentice: apprentice, trade: trade, test-id: test-id }))
      (map-set tests key
        { score: score, passed: passed, assessor: tx-sender, taken-bn: stacks-block-height })
      (ok true))))

;; Certification functions
(define-public (issue-certification (apprentice principal) (trade (string-ascii 64)) (level uint) (valid-until (optional uint)))
  (begin
    (asserts! (is-some (map-get? approved-providers tx-sender)) ERR_UNAUTHORIZED)
    (let ((key-appr { apprentice: apprentice, provider: tx-sender, trade: trade })
          (hours-required (var-get min-apprenticeship-hours)))
      (match (map-get? apprenticeships key-appr)
        apprenticeship-data
          (if (>= (get hours apprenticeship-data) hours-required)
              (begin
                (map-set certifications { apprentice: apprentice, trade: trade }
                  { level: level, issued-by: tx-sender, issued-at: stacks-block-height, valid-until: valid-until })
                (ok true))
              ERR_INVALID_INPUT)
        ERR_NOT_FOUND))))

;; Continuing education functions
(define-public (add-ce-credits (apprentice principal) (trade (string-ascii 64)) (credits uint))
  (begin
    (asserts! (is-some (map-get? approved-providers tx-sender)) ERR_UNAUTHORIZED)
    (let ((key { apprentice: apprentice, trade: trade }))
      (match (map-get? ce-credits key)
        existing-credits
          (let ((new-total (+ (get credits existing-credits) credits)))
            (map-set ce-credits key { credits: new-total, last-updated: stacks-block-height })
            (ok new-total))
        (begin
          (map-set ce-credits key { credits: credits, last-updated: stacks-block-height })
          (ok credits))))))

;; Employer recognition functions
(define-public (recognize-worker (apprentice principal) (trade (string-ascii 64)))
  (begin
    (asserts! (is-some (map-get? approved-employers tx-sender)) ERR_UNAUTHORIZED)
    (let ((key { apprentice: apprentice, employer: tx-sender, trade: trade }))
      (map-set employer-recognitions key { recognized: true, at-bn: stacks-block-height })
      (ok true))))

;; Read-only functions
(define-read-only (is-provider (provider principal))
  (is-some (map-get? approved-providers provider)))

(define-read-only (is-employer (employer principal))
  (is-some (map-get? approved-employers employer)))

(define-read-only (get-certification (apprentice principal) (trade (string-ascii 64)))
  (map-get? certifications { apprentice: apprentice, trade: trade }))

(define-read-only (verify-competency (apprentice principal) (trade (string-ascii 64)))
  (match (map-get? certifications { apprentice: apprentice, trade: trade })
    cert-data
      (match (get valid-until cert-data)
        expiry-block
          (>= expiry-block stacks-block-height)
        true)
    false))

(define-read-only (get-ce-credits (apprentice principal) (trade (string-ascii 64)))
  (map-get? ce-credits { apprentice: apprentice, trade: trade }))

(define-read-only (is-recognized-by (apprentice principal) (employer principal) (trade (string-ascii 64)))
  (match (map-get? employer-recognitions { apprentice: apprentice, employer: employer, trade: trade })
    recognition-data
      (get recognized recognition-data)
    false))

(define-read-only (get-apprenticeship (apprentice principal) (provider principal) (trade (string-ascii 64)))
  (map-get? apprenticeships { apprentice: apprentice, provider: provider, trade: trade }))

(define-read-only (get-test (apprentice principal) (trade (string-ascii 64)) (test-id uint))
  (map-get? tests { apprentice: apprentice, trade: trade, test-id: test-id }))

(define-read-only (get-min-hours)
  (var-get min-apprenticeship-hours))

(define-read-only (get-owner)
  (var-get contract-owner))
