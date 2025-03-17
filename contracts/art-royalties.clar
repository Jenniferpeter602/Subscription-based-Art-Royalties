;; title: art-royalties
;; version: 1.0
;; summary: Subscription based Art Royalties

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ARTWORK-EXISTS (err u101))
(define-constant ERR-ARTWORK-NOT-FOUND (err u102))
(define-constant SUBSCRIPTION-PRICE u10000000) ;; 10 STX

;; Data Maps
(define-map artworks
    principal
    {
        title: (string-ascii 100),
        artist: principal,
        subscription-price: uint,
        active: bool
    }
)

(define-map subscriptions
    {subscriber: principal, artist: principal}
    {
        expires-at: uint,
        active: bool
    }
)

;; Public Functions
(define-public (register-artwork (title (string-ascii 100)))
    (let
        ((artist tx-sender))
        (if (is-none (map-get? artworks artist))
            (ok (map-set artworks artist {
                title: title,
                artist: artist,
                subscription-price: SUBSCRIPTION-PRICE,
                active: true
            }))
            ERR-ARTWORK-EXISTS
        )
    )
)

(define-public (subscribe-to-artist (artist principal))
    (let
        ((subscriber tx-sender)
         (artwork (unwrap! (map-get? artworks artist) ERR-ARTWORK-NOT-FOUND))
         (current-block stacks-block-height))
        (try! (stx-transfer? SUBSCRIPTION-PRICE subscriber artist))
        (ok (map-set subscriptions 
            {subscriber: subscriber, artist: artist}
            {
                expires-at: (+ current-block u144), ;; ~1 day in blocks
                active: true
            }
        ))
    )
)

;; Read Only Functions
(define-read-only (get-artwork (artist principal))
    (map-get? artworks artist)
)

(define-read-only (check-subscription (subscriber principal) (artist principal))
    (let
        ((sub (map-get? subscriptions {subscriber: subscriber, artist: artist})))
        (if (is-none sub)
            false
            (> (get expires-at (unwrap-panic sub)) stacks-block-height)
        )
    )
)




;; Add new map for artist profiles
(define-map artist-profiles
    principal
    {
        name: (string-ascii 50),
        bio: (string-ascii 500),
        social-links: (string-ascii 200),
        total-subscribers: uint
    }
)

(define-public (create-artist-profile (name (string-ascii 50)) (bio (string-ascii 500)) (social-links (string-ascii 200)))
    (ok (map-set artist-profiles tx-sender {
        name: name,
        bio: bio,
        social-links: social-links,
        total-subscribers: u0
    }))
)



(define-constant BASIC-TIER-PRICE u10000000)    ;; 10 STX
(define-constant PREMIUM-TIER-PRICE u20000000)  ;; 20 STX
(define-constant VIP-TIER-PRICE u50000000)      ;; 50 STX

(define-public (subscribe-with-tier (artist principal) (tier uint))
    (let 
        ((price (if (is-eq tier u1) 
            BASIC-TIER-PRICE
            (if (is-eq tier u2)
                PREMIUM-TIER-PRICE
                (if (is-eq tier u3)
                    VIP-TIER-PRICE
                    BASIC-TIER-PRICE)))))
        (try! (stx-transfer? price tx-sender artist))
        (ok true))
)



(define-map collections
    {artist: principal, collection-id: uint}
    {
        name: (string-ascii 100),
        description: (string-ascii 500),
        artwork-count: uint
    }
)

(define-data-var next-collection-id uint u1)

(define-public (create-collection (name (string-ascii 100)) (description (string-ascii 500)))
    (let ((collection-id (var-get next-collection-id)))
        (map-set collections 
            {artist: tx-sender, collection-id: collection-id}
            {name: name, description: description, artwork-count: u0}
        )
        (var-set next-collection-id (+ collection-id u1))
        (ok collection-id)
    )
)



(define-map revenue-sharing
    principal
    {
        collaborators: (list 5 principal),
        shares: (list 5 uint)
    }
)

(define-public (set-revenue-sharing (collaborators (list 5 principal)) (shares (list 5 uint)))
    (ok (map-set revenue-sharing tx-sender {
        collaborators: collaborators,
        shares: shares
    }))
)



(define-map special-offers
    principal
    {
        discount-price: uint,
        start-block: uint,
        end-block: uint,
        active: bool
    }
)

(define-public (create-special-offer (discount-price uint) (duration uint))
    (ok (map-set special-offers tx-sender {
        discount-price: discount-price,
        start-block: stacks-block-height,
        end-block: (+ stacks-block-height duration),
        active: true
    }))
)


(define-map subscriber-analytics
    principal
    {
        total-subscriptions: uint,
        subscription-history: (list 10 uint),
        last-active: uint
    }
)

(define-read-only (get-subscriber-stats (subscriber principal))
    (default-to 
        {total-subscriptions: u0, subscription-history: (list ), last-active: u0}
        (map-get? subscriber-analytics subscriber)
    )
)



(define-map gift-subscriptions
    {sender: principal, recipient: principal, artist: principal}
    {
        created-at: uint,
        duration: uint,
        redeemed: bool
    }
)

(define-public (gift-subscription (recipient principal) (artist principal))
    (let ((sender tx-sender))
        (try! (stx-transfer? SUBSCRIPTION-PRICE sender artist))
        (ok (map-set gift-subscriptions
            {sender: sender, recipient: recipient, artist: artist}
            {created-at: stacks-block-height, duration: u144, redeemed: false}
        ))
    )
)


;; Timed Exclusive Content
(define-map exclusive-content
    {artist: principal, content-id: uint}
    {
        title: (string-ascii 100),
        description: (string-ascii 500),
        release-block: uint,
        expiry-block: uint,
        active: bool
    }
)

(define-data-var next-content-id uint u1)

(define-public (create-exclusive-content 
    (title (string-ascii 100)) 
    (description (string-ascii 500)) 
    (duration uint))
    (let 
        ((content-id (var-get next-content-id))
         (current-block stacks-block-height))
        (map-set exclusive-content
            {artist: tx-sender, content-id: content-id}
            {
                title: title,
                description: description,
                release-block: current-block,
                expiry-block: (+ current-block duration),
                active: true
            }
        )
        (var-set next-content-id (+ content-id u1))
        (ok content-id)
    )
)

(define-read-only (get-exclusive-content (artist principal) (content-id uint))
    (let 
        ((content (map-get? exclusive-content {artist: artist, content-id: content-id}))
         (current-block stacks-block-height))
        (if (and 
                (is-some content) 
                (get active (unwrap-panic content))
                (<= (get expiry-block (unwrap-panic content)) current-block))
            content
            none
        )
    )
)



;; Subscription Renewal Reminders
(define-map renewal-preferences
    principal
    {
        reminder-blocks: uint,  ;; How many blocks before expiry to remind
        auto-renew: bool
    }
)

(define-public (set-renewal-preferences (reminder-blocks uint) (auto-renew bool))
    (ok (map-set renewal-preferences tx-sender {
        reminder-blocks: reminder-blocks,
        auto-renew: auto-renew
    }))
)

(define-read-only (check-renewal-needed (subscriber principal) (artist principal))
    (let
        ((sub (map-get? subscriptions {subscriber: subscriber, artist: artist}))
         (prefs (map-get? renewal-preferences subscriber))
         (current-block stacks-block-height))
        (if (or (is-none sub) (is-none prefs))
            false
            (let 
                ((expiry (get expires-at (unwrap-panic sub)))
                 (reminder-threshold (get reminder-blocks (unwrap-panic prefs))))
                (and 
                    (<= (- expiry reminder-threshold) current-block)
                    (> expiry current-block)
                )
            )
        )
    )
)

(define-public (auto-renew-subscription (artist principal))
    (let
        ((subscriber tx-sender)
         (prefs (default-to {reminder-blocks: u0, auto-renew: false} 
                 (map-get? renewal-preferences subscriber)))
         (sub (map-get? subscriptions {subscriber: subscriber, artist: artist}))
         (current-block stacks-block-height))
        (asserts! (get auto-renew prefs) (err u103))
        (asserts! (is-some sub) ERR-ARTWORK-NOT-FOUND)
        (try! (stx-transfer? SUBSCRIPTION-PRICE subscriber artist))
        (ok (map-set subscriptions 
            {subscriber: subscriber, artist: artist}
            {
                expires-at: (+ current-block u144), ;; ~1 day in blocks
                active: true
            }
        ))
    )
)


;; Enhanced Subscription Tiers
(define-map subscription-tier-details
    {artist: principal, tier-id: uint}
    {
        name: (string-ascii 50),
        price: uint,
        duration: uint,  ;; in blocks
        benefits: (string-ascii 200),
        active: bool
    }
)

(define-public (create-subscription-tier 
    (name (string-ascii 50)) 
    (price uint) 
    (duration uint) 
    (benefits (string-ascii 200)))
    (let ((artist tx-sender))
        (ok (map-set subscription-tier-details
            {artist: artist, tier-id: (if (is-eq price BASIC-TIER-PRICE) u1 
                                       (if (is-eq price PREMIUM-TIER-PRICE) u2 u3))}
            {
                name: name,
                price: price,
                duration: duration,
                benefits: benefits,
                active: true
            }
        ))
    )
)

(define-public (subscribe-to-tier-with-benefits (artist principal) (tier-id uint))
    (let
        ((subscriber tx-sender)
         (tier (unwrap! (map-get? subscription-tier-details 
                        {artist: artist, tier-id: tier-id}) 
                (err u106)))
         (current-block stacks-block-height))
        
        (asserts! (get active tier) (err u107))
        (try! (stx-transfer? (get price tier) subscriber artist))
        
        (ok (map-set subscriptions 
            {subscriber: subscriber, artist: artist}
            {
                expires-at: (+ current-block (get duration tier)),
                active: true
            }
        ))
    )
)

(define-read-only (get-subscription-tier-details (artist principal) (tier-id uint))
    (map-get? subscription-tier-details {artist: artist, tier-id: tier-id})
)


;; Artist Feedback and Rating System
(define-map artist-ratings
    principal
    {
        total-ratings: uint,
        rating-sum: uint,
        feedback-count: uint
    }
)

(define-map subscriber-feedback
    {subscriber: principal, artist: principal}
    {
        rating: uint,  ;; 1-5 scale
        feedback: (string-ascii 500),
        timestamp: uint
    }
)

(define-public (rate-artist (artist principal) (rating uint) (feedback (string-ascii 500)))
    (let
        ((subscriber tx-sender)
         (current-block stacks-block-height)
         (current-ratings (default-to 
                          {total-ratings: u0, rating-sum: u0, feedback-count: u0} 
                          (map-get? artist-ratings artist))))
        
        ;; Ensure rating is between 1-5
        (asserts! (and (>= rating u1) (<= rating u5)) (err u108))
        
        ;; Ensure subscriber has an active subscription
        (asserts! (check-subscription subscriber artist) (err u109))
        
        ;; Record the feedback
        (map-set subscriber-feedback 
            {subscriber: subscriber, artist: artist}
            {
                rating: rating,
                feedback: feedback,
                timestamp: current-block
            }
        )
        
        ;; Update artist ratings
        (ok (map-set artist-ratings artist {
            total-ratings: (+ (get total-ratings current-ratings) u1),
            rating-sum: (+ (get rating-sum current-ratings) rating),
            feedback-count: (+ (get feedback-count current-ratings) u1)
        }))
    )
)

(define-read-only (get-artist-average-rating (artist principal))
    (let ((ratings (default-to 
                  {total-ratings: u0, rating-sum: u0, feedback-count: u0} 
                  (map-get? artist-ratings artist))))
        (if (is-eq (get total-ratings ratings) u0)
            u0
            (/ (get rating-sum ratings) (get total-ratings ratings))
        )
    )
)

(define-read-only (get-subscriber-feedback (subscriber principal) (artist principal))
    (map-get? subscriber-feedback {subscriber: subscriber, artist: artist})
)



;; Referral Program
(define-map referrals
    {referrer: principal, referred: principal}
    {
        timestamp: uint,
        artist: principal,
        reward-claimed: bool
    }
)

(define-map referral-stats
    principal
    {
        total-referrals: uint,
        rewards-earned: uint
    }
)

(define-constant REFERRAL-REWARD-PERCENTAGE u10)  ;; 10% of subscription price

(define-public (subscribe-with-referral (artist principal) (referrer principal))
    (let
        ((subscriber tx-sender)
         (current-block stacks-block-height)
         (referrer-stats (default-to 
                         {total-referrals: u0, rewards-earned: u0} 
                         (map-get? referral-stats referrer))))
        
        ;; Ensure referrer is not the same as subscriber
        (asserts! (not (is-eq subscriber referrer)) (err u110))
        
        ;; Process subscription payment
        (try! (stx-transfer? SUBSCRIPTION-PRICE subscriber artist))
        
        ;; Record subscription
        (map-set subscriptions 
            {subscriber: subscriber, artist: artist}
            {
                expires-at: (+ current-block u144),
                active: true
            }
        )
        
        ;; Record referral
        (map-set referrals
            {referrer: referrer, referred: subscriber}
            {
                timestamp: current-block,
                artist: artist,
                reward-claimed: false
            }
        )
        
        ;; Update referrer stats
        (ok (map-set referral-stats referrer {
            total-referrals: (+ (get total-referrals referrer-stats) u1),
            rewards-earned: (get rewards-earned referrer-stats)
        }))
    )
)

(define-public (claim-referral-reward (referred principal))
    (let
        ((referrer tx-sender)
         (referral (unwrap! (map-get? referrals 
                           {referrer: referrer, referred: referred}) 
                  (err u111)))
         (artist (get artist referral))
         (reward-amount (/ (* SUBSCRIPTION-PRICE REFERRAL-REWARD-PERCENTAGE) u100))
         (referrer-stats (default-to 
                         {total-referrals: u0, rewards-earned: u0} 
                         (map-get? referral-stats referrer))))
        
        ;; Ensure reward hasn't been claimed yet
        (asserts! (not (get reward-claimed referral)) (err u112))
        
        ;; Transfer reward from artist to referrer
        (try! (stx-transfer? reward-amount artist referrer))
        
        ;; Mark referral as claimed
        (map-set referrals
            {referrer: referrer, referred: referred}
            (merge referral {reward-claimed: true})
        )
        
        ;; Update referrer stats
        (ok (map-set referral-stats referrer {
            total-referrals: (get total-referrals referrer-stats),
            rewards-earned: (+ (get rewards-earned referrer-stats) reward-amount)
        }))
    )
)

(define-read-only (get-referral-stats (referrer principal))
    (default-to 
        {total-referrals: u0, rewards-earned: u0}
        (map-get? referral-stats referrer)
    )
)
