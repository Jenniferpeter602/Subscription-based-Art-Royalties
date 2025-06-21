(define-constant ERR-NOT-AUTHORIZED (err u600))
(define-constant ERR-CONTENT-NOT-FOUND (err u601))
(define-constant ERR-ACCESS-DENIED (err u602))
(define-constant ERR-CONTENT-LOCKED (err u603))
(define-constant ERR-INVALID-UNLOCK-TIME (err u604))

(define-map protected-content
    {artist: principal, content-id: uint}
    {
        title: (string-ascii 100),
        content-hash: (string-ascii 64),
        unlock-block: uint,
        required-tier: uint,
        access-count: uint,
        max-access: uint,
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

(define-map content-access-log
    {subscriber: principal, artist: principal, content-id: uint}
    {
        first-access: uint,
        access-count: uint,
        last-access: uint
    }
)

(define-data-var next-protected-content-id uint u1)

(define-public (create-protected-content 
    (title (string-ascii 100))
    (content-hash (string-ascii 64))
    (unlock-delay uint)
    (required-tier uint)
    (max-access uint))
    (let 
        ((content-id (var-get next-protected-content-id))
         (unlock-block (+ stacks-block-height unlock-delay)))
        (asserts! (> unlock-delay u0) ERR-INVALID-UNLOCK-TIME)
        (var-set next-protected-content-id (+ content-id u1))
        (ok (map-set protected-content
            {artist: tx-sender, content-id: content-id}
            {
                title: title,
                content-hash: content-hash,
                unlock-block: unlock-block,
                required-tier: required-tier,
                access-count: u0,
                max-access: max-access,
                active: true
            }))
    )
)

(define-public (access-protected-content (artist principal) (content-id uint))
    (let 
        ((content (unwrap! (map-get? protected-content {artist: artist, content-id: content-id}) ERR-CONTENT-NOT-FOUND))
         (subscriber tx-sender)
         (current-block stacks-block-height)
         (access-log (default-to 
                     {first-access: u0, access-count: u0, last-access: u0}
                     (map-get? content-access-log {subscriber: subscriber, artist: artist, content-id: content-id}))))
        (asserts! (get active content) ERR-CONTENT-NOT-FOUND)
        (asserts! (>= current-block (get unlock-block content)) ERR-CONTENT-LOCKED)
        (asserts! (< (get access-count content) (get max-access content)) ERR-ACCESS-DENIED)
        (asserts! (is-subscriber-active subscriber artist) ERR-ACCESS-DENIED)
        (map-set content-access-log
            {subscriber: subscriber, artist: artist, content-id: content-id}
            {
                first-access: (if (is-eq (get first-access access-log) u0) current-block (get first-access access-log)),
                access-count: (+ (get access-count access-log) u1),
                last-access: current-block
            }
        )
        (ok (map-set protected-content
            {artist: artist, content-id: content-id}
            (merge content {access-count: (+ (get access-count content) u1)})
        ))
    )
)

(define-public (update-content-unlock-time (content-id uint) (new-unlock-delay uint))
    (let 
        ((content (unwrap! (map-get? protected-content {artist: tx-sender, content-id: content-id}) ERR-CONTENT-NOT-FOUND))
         (new-unlock-block (+ stacks-block-height new-unlock-delay)))
        (asserts! (> new-unlock-delay u0) ERR-INVALID-UNLOCK-TIME)
        (ok (map-set protected-content
            {artist: tx-sender, content-id: content-id}
            (merge content {unlock-block: new-unlock-block})
        ))
    )
)

(define-public (deactivate-content (content-id uint))
    (let ((content (unwrap! (map-get? protected-content {artist: tx-sender, content-id: content-id}) ERR-CONTENT-NOT-FOUND)))
        (ok (map-set protected-content
            {artist: tx-sender, content-id: content-id}
            (merge content {active: false})
        ))
    )
)

(define-read-only (get-protected-content (artist principal) (content-id uint))
    (map-get? protected-content {artist: artist, content-id: content-id})
)

(define-read-only (is-content-unlocked (artist principal) (content-id uint))
    (let ((content (map-get? protected-content {artist: artist, content-id: content-id})))
        (if (is-none content)
            false
            (>= stacks-block-height (get unlock-block (unwrap-panic content)))
        )
    )
)

(define-read-only (can-access-content (subscriber principal) (artist principal) (content-id uint))
    (let 
        ((content (map-get? protected-content {artist: artist, content-id: content-id}))
         (current-block stacks-block-height))
        (if (is-none content)
            false
            (let ((content-data (unwrap-panic content)))
                (and 
                    (get active content-data)
                    (>= current-block (get unlock-block content-data))
                    (< (get access-count content-data) (get max-access content-data))
                    (is-subscriber-active subscriber artist)
                )
            )
        )
    )
)

(define-read-only (get-content-access-stats (subscriber principal) (artist principal) (content-id uint))
    (map-get? content-access-log {subscriber: subscriber, artist: artist, content-id: content-id})
)

(define-read-only (get-time-until-unlock (artist principal) (content-id uint))
    (let ((content (map-get? protected-content {artist: artist, content-id: content-id})))
        (if (is-none content)
            (some u0)
            (let 
                ((unlock-block (get unlock-block (unwrap-panic content)))
                 (current-block stacks-block-height))
                (if (>= current-block unlock-block)
                    (some u0)
                    (some (- unlock-block current-block))
                )
            )
        )
    )
)

(define-read-only (is-subscriber-active (subscriber principal) (artist principal))
    (let ((sub (map-get? subscriptions {subscriber: subscriber, artist: artist})))
        (if (is-none sub)
            false
            (> (get expires-at (unwrap-panic sub)) stacks-block-height)
        )
    )
)

(define-read-only (list-artist-content (artist principal))
    (let ((content-id (var-get next-protected-content-id)))
        (map get-protected-content (list artist artist artist artist artist) (list u1 u2 u3 u4 u5))
    )
)