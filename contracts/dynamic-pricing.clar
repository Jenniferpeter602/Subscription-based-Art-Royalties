(define-constant ERR-NOT-AUTHORIZED (err u700))
(define-constant ERR-INVALID-PRICE (err u701))
(define-constant ERR-INVALID-TIME-RANGE (err u702))
(define-constant ERR-SCHEDULE-NOT-FOUND (err u703))
(define-constant ERR-SCHEDULE-CONFLICT (err u704))
(define-constant ERR-PRICE-SCHEDULE-EXPIRED (err u705))

(define-map pricing-schedules
    {artist: principal, schedule-id: uint}
    {
        base-price: uint,
        multiplier: uint,
        start-block: uint,
        end-block: uint,
        tier-id: uint,
        active: bool
    }
)

(define-map current-pricing
    {artist: principal, tier-id: uint}
    {
        current-price: uint,
        last-updated: uint,
        active-schedule-id: uint
    }
)

(define-map artist-pricing-stats
    {artist: principal}
    {
        total-schedules: uint,
        revenue-boost: uint,
        price-changes: uint
    }
)

(define-data-var next-schedule-id uint u1)

(define-public (create-pricing-schedule 
    (base-price uint)
    (multiplier uint)
    (start-delay uint)
    (duration uint)
    (tier-id uint))
    (let 
        ((schedule-id (var-get next-schedule-id))
         (start-block (+ stacks-block-height start-delay))
         (end-block (+ start-block duration)))
        (asserts! (> base-price u0) ERR-INVALID-PRICE)
        (asserts! (and (>= multiplier u50) (<= multiplier u300)) ERR-INVALID-PRICE)
        (asserts! (> duration u0) ERR-INVALID-TIME-RANGE)
        (asserts! (> start-delay u0) ERR-INVALID-TIME-RANGE)
        (var-set next-schedule-id (+ schedule-id u1))
        (ok (map-set pricing-schedules
            {artist: tx-sender, schedule-id: schedule-id}
            {
                base-price: base-price,
                multiplier: multiplier,
                start-block: start-block,
                end-block: end-block,
                tier-id: tier-id,
                active: true
            }))
    )
)

(define-public (update-pricing-schedule 
    (schedule-id uint)
    (new-multiplier uint))
    (let 
        ((schedule (unwrap! (map-get? pricing-schedules {artist: tx-sender, schedule-id: schedule-id}) ERR-SCHEDULE-NOT-FOUND)))
        (asserts! (get active schedule) ERR-SCHEDULE-NOT-FOUND)
        (asserts! (> (get start-block schedule) stacks-block-height) ERR-PRICE-SCHEDULE-EXPIRED)
        (asserts! (and (>= new-multiplier u50) (<= new-multiplier u300)) ERR-INVALID-PRICE)
        (ok (map-set pricing-schedules
            {artist: tx-sender, schedule-id: schedule-id}
            (merge schedule {multiplier: new-multiplier})
        ))
    )
)

(define-public (activate-pricing-schedule (schedule-id uint))
    (let 
        ((schedule (unwrap! (map-get? pricing-schedules {artist: tx-sender, schedule-id: schedule-id}) ERR-SCHEDULE-NOT-FOUND))
         (current-block stacks-block-height))
        (asserts! (and (>= current-block (get start-block schedule)) 
                       (<= current-block (get end-block schedule))) ERR-PRICE-SCHEDULE-EXPIRED)
        (asserts! (get active schedule) ERR-SCHEDULE-NOT-FOUND)
        (let 
            ((new-price (/ (* (get base-price schedule) (get multiplier schedule)) u100))
             (stats (default-to 
                    {total-schedules: u0, revenue-boost: u0, price-changes: u0}
                    (map-get? artist-pricing-stats {artist: tx-sender}))))
            (map-set current-pricing
                {artist: tx-sender, tier-id: (get tier-id schedule)}
                {
                    current-price: new-price,
                    last-updated: current-block,
                    active-schedule-id: schedule-id
                }
            )
            (ok (map-set artist-pricing-stats
                {artist: tx-sender}
                {
                    total-schedules: (+ (get total-schedules stats) u1),
                    revenue-boost: (get revenue-boost stats),
                    price-changes: (+ (get price-changes stats) u1)
                }
            ))
        )
    )
)

(define-public (deactivate-pricing-schedule (schedule-id uint))
    (let 
        ((schedule (unwrap! (map-get? pricing-schedules {artist: tx-sender, schedule-id: schedule-id}) ERR-SCHEDULE-NOT-FOUND)))
        (ok (map-set pricing-schedules
            {artist: tx-sender, schedule-id: schedule-id}
            (merge schedule {active: false})
        ))
    )
)

(define-public (subscribe-with-dynamic-pricing (artist principal) (tier-id uint))
    (let 
        ((current-price-data (unwrap! (map-get? current-pricing {artist: artist, tier-id: tier-id}) ERR-SCHEDULE-NOT-FOUND))
         (price (get current-price current-price-data))
         (subscriber tx-sender)
         (current-block stacks-block-height))
        (try! (stx-transfer? price subscriber artist))
        (ok true)
    )
)

(define-read-only (get-current-price (artist principal) (tier-id uint))
    (let 
        ((pricing-data (map-get? current-pricing {artist: artist, tier-id: tier-id})))
        (if (is-some pricing-data)
            (some (get current-price (unwrap-panic pricing-data)))
            none
        )
    )
)

(define-read-only (get-pricing-schedule (artist principal) (schedule-id uint))
    (map-get? pricing-schedules {artist: artist, schedule-id: schedule-id})
)

(define-read-only (is-schedule-active (artist principal) (schedule-id uint))
    (let 
        ((schedule (map-get? pricing-schedules {artist: artist, schedule-id: schedule-id}))
         (current-block stacks-block-height))
        (if (is-some schedule)
            (let ((schedule-data (unwrap-panic schedule)))
                (and 
                    (get active schedule-data)
                    (>= current-block (get start-block schedule-data))
                    (<= current-block (get end-block schedule-data))
                )
            )
            false
        )
    )
)

(define-read-only (calculate-future-price (artist principal) (schedule-id uint))
    (let 
        ((schedule (map-get? pricing-schedules {artist: artist, schedule-id: schedule-id})))
        (if (is-some schedule)
            (let ((schedule-data (unwrap-panic schedule)))
                (some (/ (* (get base-price schedule-data) (get multiplier schedule-data)) u100))
            )
            none
        )
    )
)

(define-read-only (get-artist-pricing-stats (artist principal))
    (default-to 
        {total-schedules: u0, revenue-boost: u0, price-changes: u0}
        (map-get? artist-pricing-stats {artist: artist})
    )
)

(define-read-only (get-price-history (artist principal) (tier-id uint))
    (map-get? current-pricing {artist: artist, tier-id: tier-id})
)

(define-read-only (get-next-schedule-id)
    (var-get next-schedule-id)
)

(define-public (bulk-create-pricing-schedules 
    (schedules (list 5 {base-price: uint, multiplier: uint, start-delay: uint, duration: uint, tier-id: uint})))
    (let 
        ((results (map create-single-schedule schedules)))
        (ok results)
    )
)

(define-private (create-single-schedule (schedule-data {base-price: uint, multiplier: uint, start-delay: uint, duration: uint, tier-id: uint}))
    (let 
        ((schedule-id (var-get next-schedule-id))
         (start-block (+ stacks-block-height (get start-delay schedule-data)))
         (end-block (+ start-block (get duration schedule-data))))
        (var-set next-schedule-id (+ schedule-id u1))
        (map-set pricing-schedules
            {artist: tx-sender, schedule-id: schedule-id}
            {
                base-price: (get base-price schedule-data),
                multiplier: (get multiplier schedule-data),
                start-block: start-block,
                end-block: end-block,
                tier-id: (get tier-id schedule-data),
                active: true
            }
        )
        schedule-id
    )
)

(define-public (emergency-reset-pricing (tier-id uint) (fallback-price uint))
    (let 
        ((current-block stacks-block-height))
        (asserts! (> fallback-price u0) ERR-INVALID-PRICE)
        (ok (map-set current-pricing
            {artist: tx-sender, tier-id: tier-id}
            {
                current-price: fallback-price,
                last-updated: current-block,
                active-schedule-id: u0
            }
        ))
    )
)
