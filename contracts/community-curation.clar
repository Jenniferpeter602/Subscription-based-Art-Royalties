;; title: community-curation
;; version: 1.0
;; summary: Community-driven art curation and voting system

;; Constants
(define-constant ERR-NOT-AUTHORIZED (err u800))
(define-constant ERR-CURATION-NOT-FOUND (err u801))
(define-constant ERR-ALREADY-VOTED (err u802))
(define-constant ERR-VOTING-CLOSED (err u803))
(define-constant ERR-INVALID-PARAMETERS (err u804))
(define-constant ERR-INSUFFICIENT-VOTING-POWER (err u805))
(define-constant ERR-CURATOR-NOT-QUALIFIED (err u806))
(define-constant ERR-PROPOSAL-ALREADY-EXECUTED (err u807))
(define-constant ERR-PROPOSAL-NOT-PASSED (err u808))

;; Voting thresholds and parameters
(define-constant MIN-VOTING-POWER u100)
(define-constant VOTING-DURATION u144) ;; ~1 day in blocks
(define-constant APPROVAL-THRESHOLD u60) ;; 60% approval needed
(define-constant BASIC-REPUTATION u10)
(define-constant ADVANCED-REPUTATION u50)

;; Data structures for curation proposals
(define-map curation-proposals
    { proposal-id: uint }
    {
        proposer: principal,
        artist: principal,
        proposal-type: uint, ;; 1: feature artwork, 2: create collection, 3: special event
        title: (string-ascii 100),
        description: (string-ascii 500),
        metadata-hash: (string-ascii 64),
        voting-start: uint,
        voting-end: uint,
        total-yes-votes: uint,
        total-no-votes: uint,
        total-voting-power: uint,
        status: uint, ;; 0: active, 1: passed, 2: rejected, 3: executed
        execution-block: uint,
    }
)

;; Track individual votes on proposals
(define-map proposal-votes
    {
        proposal-id: uint,
        voter: principal,
    }
    {
        vote: bool, ;; true for yes, false for no
        voting-power: uint,
        timestamp: uint,
    }
)

;; Community curator profiles with reputation system
(define-map curator-profiles
    { curator: principal }
    {
        reputation-score: uint,
        total-proposals: uint,
        successful-proposals: uint,
        total-votes-cast: uint,
        community-tier: uint, ;; 1: novice, 2: experienced, 3: expert
        last-active: uint,
    }
)

;; Track subscription-based voting power for each voter-artist pair
(define-map voter-power
    {
        voter: principal,
        artist: principal,
    }
    {
        base-power: uint,
        tier-multiplier: uint,
        subscription-duration: uint,
        loyalty-bonus: uint,
        total-power: uint,
    }
)

;; Featured artwork collections curated by community
(define-map curated-collections
    { collection-id: uint }
    {
        name: (string-ascii 100),
        curator: principal,
        featured-artists: (list 10 principal),
        votes-received: uint,
        creation-block: uint,
        featured-until: uint,
        active: bool,
    }
)

;; Community events and contests organized through proposals
(define-map community-events
    { event-id: uint }
    {
        organizer: principal,
        event-type: uint, ;; 1: contest, 2: exhibition, 3: collaboration
        title: (string-ascii 100),
        description: (string-ascii 500),
        start-block: uint,
        end-block: uint,
        participation-fee: uint,
        prize-pool: uint,
        max-participants: uint,
        current-participants: uint,
        winner: (optional principal),
        status: uint, ;; 0: upcoming, 1: active, 2: ended, 3: prizes-distributed
    }
)

;; Track participation in community events
(define-map event-participants
    {
        event-id: uint,
        participant: principal,
    }
    {
        entry-block: uint,
        submission-hash: (string-ascii 64),
        votes-received: uint,
    }
)

;; Data variables for auto-incrementing IDs
(define-data-var next-proposal-id uint u1)
(define-data-var next-collection-id uint u1)
(define-data-var next-event-id uint u1)

;; Public function to create curation proposals
(define-public (create-curation-proposal
        (artist principal)
        (proposal-type uint)
        (title (string-ascii 100))
        (description (string-ascii 500))
        (metadata-hash (string-ascii 64))
    )
    (let (
            (proposal-id (var-get next-proposal-id))
            (current-block stacks-block-height)
            (proposer tx-sender)
            (curator-profile (default-to {
                reputation-score: u0,
                total-proposals: u0,
                successful-proposals: u0,
                total-votes-cast: u0,
                community-tier: u1,
                last-active: u0,
            }
                (map-get? curator-profiles { curator: proposer })
            ))
        )
        ;; Check if proposer has sufficient reputation for advanced proposal types
        (asserts!
            (or
                (is-eq proposal-type u1)
                (>= (get reputation-score curator-profile) ADVANCED-REPUTATION)
            )
            ERR-CURATOR-NOT-QUALIFIED
        )
        (asserts! (and (>= proposal-type u1) (<= proposal-type u3))
            ERR-INVALID-PARAMETERS
        )

        (var-set next-proposal-id (+ proposal-id u1))

        ;; Create the proposal
        (map-set curation-proposals { proposal-id: proposal-id } {
            proposer: proposer,
            artist: artist,
            proposal-type: proposal-type,
            title: title,
            description: description,
            metadata-hash: metadata-hash,
            voting-start: current-block,
            voting-end: (+ current-block VOTING-DURATION),
            total-yes-votes: u0,
            total-no-votes: u0,
            total-voting-power: u0,
            status: u0,
            execution-block: u0,
        })

        ;; Update curator profile
        (map-set curator-profiles { curator: proposer }
            (merge curator-profile {
                total-proposals: (+ (get total-proposals curator-profile) u1),
                last-active: current-block,
            })
        )

        (ok proposal-id)
    )
)

;; Public function for voting on proposals with subscription-based power
(define-public (vote-on-proposal
        (proposal-id uint)
        (vote bool)
    )
    (let (
            (proposal (unwrap! (map-get? curation-proposals { proposal-id: proposal-id })
                ERR-CURATION-NOT-FOUND
            ))
            (voter tx-sender)
            (current-block stacks-block-height)
            (voting-power-data (calculate-voting-power voter (get artist proposal)))
        )
        ;; Check voting eligibility
        (asserts! (<= current-block (get voting-end proposal)) ERR-VOTING-CLOSED)
        (asserts! (is-eq (get status proposal) u0) ERR-VOTING-CLOSED)
        (asserts!
            (is-none (map-get? proposal-votes {
                proposal-id: proposal-id,
                voter: voter,
            }))
            ERR-ALREADY-VOTED
        )
        (asserts! (>= voting-power-data MIN-VOTING-POWER)
            ERR-INSUFFICIENT-VOTING-POWER
        )

        ;; Record the vote
        (map-set proposal-votes {
            proposal-id: proposal-id,
            voter: voter,
        } {
            vote: vote,
            voting-power: voting-power-data,
            timestamp: current-block,
        })

        ;; Update proposal vote counts
        (let (
                (new-yes-votes (if vote
                    (+ (get total-yes-votes proposal) voting-power-data)
                    (get total-yes-votes proposal)
                ))
                (new-no-votes (if vote
                    (get total-no-votes proposal)
                    (+ (get total-no-votes proposal) voting-power-data)
                ))
                (new-total-power (+ (get total-voting-power proposal) voting-power-data))
            )
            (map-set curation-proposals { proposal-id: proposal-id }
                (merge proposal {
                    total-yes-votes: new-yes-votes,
                    total-no-votes: new-no-votes,
                    total-voting-power: new-total-power,
                })
            )
        )

        ;; Update voter's profile
        (let ((curator-profile (default-to {
                reputation-score: u0,
                total-proposals: u0,
                successful-proposals: u0,
                total-votes-cast: u0,
                community-tier: u1,
                last-active: u0,
            }
                (map-get? curator-profiles { curator: voter })
            )))
            (map-set curator-profiles { curator: voter }
                (merge curator-profile {
                    total-votes-cast: (+ (get total-votes-cast curator-profile) u1),
                    last-active: current-block,
                    reputation-score: (+ (get reputation-score curator-profile) u1),
                })
            )
        )

        (ok voting-power-data)
    )
)

;; Public function to execute approved proposals
(define-public (execute-proposal (proposal-id uint))
    (let (
            (proposal (unwrap! (map-get? curation-proposals { proposal-id: proposal-id })
                ERR-CURATION-NOT-FOUND
            ))
            (current-block stacks-block-height)
        )
        ;; Check execution eligibility
        (asserts! (> current-block (get voting-end proposal)) ERR-VOTING-CLOSED)
        (asserts! (is-eq (get status proposal) u0) ERR-PROPOSAL-ALREADY-EXECUTED)

        ;; Check if proposal passed
        (let (
                (total-votes (get total-voting-power proposal))
                (yes-votes (get total-yes-votes proposal))
                (approval-rate (if (> total-votes u0)
                    (/ (* yes-votes u100) total-votes)
                    u0
                ))
            )
            (if (>= approval-rate APPROVAL-THRESHOLD)
                ;; Proposal passed - execute and return success
                (let (
                        (collection-id (var-get next-collection-id))
                        (proposer (get proposer proposal))
                        (profile (default-to {
                            reputation-score: u0,
                            total-proposals: u0,
                            successful-proposals: u0,
                            total-votes-cast: u0,
                            community-tier: u1,
                            last-active: u0,
                        }
                            (map-get? curator-profiles { curator: proposer })
                        ))
                    )
                    ;; Update proposal status
                    (map-set curation-proposals { proposal-id: proposal-id }
                        (merge proposal {
                            status: u1,
                            execution-block: current-block,
                        })
                    )

                    ;; Create featured collection
                    (var-set next-collection-id (+ collection-id u1))
                    (map-set curated-collections { collection-id: collection-id } {
                        name: (get title proposal),
                        curator: proposer,
                        featured-artists: (list (get artist proposal)),
                        votes-received: yes-votes,
                        creation-block: current-block,
                        featured-until: (+ current-block u1008),
                        active: true,
                    })

                    ;; Update proposer reputation
                    (map-set curator-profiles { curator: proposer }
                        (merge profile {
                            successful-proposals: (+ (get successful-proposals profile) u1),
                            reputation-score: (+ (get reputation-score profile) BASIC-REPUTATION),
                            community-tier: (calculate-community-tier (+ (get reputation-score profile) BASIC-REPUTATION)),
                        })
                    )
                    (ok true)
                )
                ;; Proposal rejected - update status and return failure
                (begin
                    (map-set curation-proposals { proposal-id: proposal-id }
                        (merge proposal { status: u2 })
                    )
                    (map-set curator-profiles { curator: (get proposer proposal) }
                        (default-to {
                            reputation-score: u0,
                            total-proposals: u0,
                            successful-proposals: u0,
                            total-votes-cast: u0,
                            community-tier: u1,
                            last-active: u0,
                        }
                            (map-get? curator-profiles { curator: (get proposer proposal) })
                        ))
                    (ok false)
                )
            )
        )
    )
)

;; Public function to create community events
(define-public (create-community-event
        (event-type uint)
        (title (string-ascii 100))
        (description (string-ascii 500))
        (duration uint)
        (participation-fee uint)
        (max-participants uint)
    )
    (let (
            (event-id (var-get next-event-id))
            (current-block stacks-block-height)
        )
        (asserts! (and (>= event-type u1) (<= event-type u3))
            ERR-INVALID-PARAMETERS
        )
        (asserts! (> duration u0) ERR-INVALID-PARAMETERS)

        (var-set next-event-id (+ event-id u1))

        (ok (map-set community-events { event-id: event-id } {
            organizer: tx-sender,
            event-type: event-type,
            title: title,
            description: description,
            start-block: current-block,
            end-block: (+ current-block duration),
            participation-fee: participation-fee,
            prize-pool: u0,
            max-participants: max-participants,
            current-participants: u0,
            winner: none,
            status: u0,
        }))
    )
)

;; Public function to participate in community events
(define-public (participate-in-event
        (event-id uint)
        (submission-hash (string-ascii 64))
    )
    (let (
            (event (unwrap! (map-get? community-events { event-id: event-id })
                ERR-CURATION-NOT-FOUND
            ))
            (participant tx-sender)
            (current-block stacks-block-height)
        )
        ;; Check participation eligibility
        (asserts! (is-eq (get status event) u1) ERR-VOTING-CLOSED)
        (asserts! (<= current-block (get end-block event)) ERR-VOTING-CLOSED)
        (asserts!
            (< (get current-participants event) (get max-participants event))
            ERR-INVALID-PARAMETERS
        )

        ;; Process participation fee if required
        (if (> (get participation-fee event) u0)
            (try! (stx-transfer? (get participation-fee event) participant
                (get organizer event)
            ))
            true
        )

        ;; Record participation
        (map-set event-participants {
            event-id: event-id,
            participant: participant,
        } {
            entry-block: current-block,
            submission-hash: submission-hash,
            votes-received: u0,
        })

        ;; Update event participant count
        (ok (map-set community-events { event-id: event-id }
            (merge event {
                current-participants: (+ (get current-participants event) u1),
                prize-pool: (+ (get prize-pool event) (get participation-fee event)),
            })
        ))
    )
)

;; Private function to calculate voting power based on subscription status and loyalty
(define-private (calculate-voting-power
        (voter principal)
        (artist principal)
    )
    (let ((power-data (default-to {
            base-power: u0,
            tier-multiplier: u100,
            subscription-duration: u0,
            loyalty-bonus: u0,
            total-power: u0,
        }
            (map-get? voter-power {
                voter: voter,
                artist: artist,
            })
        )))
        ;; Calculate power based on subscription status and loyalty
        (let (
                (base (if (> (get base-power power-data) u0)
                    (get base-power power-data)
                    u100
                ))
                (tier-bonus (get tier-multiplier power-data))
                (loyalty (get loyalty-bonus power-data))
            )
            (/ (* base tier-bonus) u100)
        )
    )
)

;; Private function to calculate community tier based on reputation
(define-private (calculate-community-tier (reputation uint))
    (if (>= reputation u200)
        u3 ;; Expert
        (if (>= reputation ADVANCED-REPUTATION)
            u2 ;; Experienced  
            u1 ;; Novice
        )
    )
)

;; Read-only function to get proposal details
(define-read-only (get-proposal (proposal-id uint))
    (map-get? curation-proposals { proposal-id: proposal-id })
)

;; Read-only function to get vote details for a specific voter and proposal
(define-read-only (get-proposal-vote
        (proposal-id uint)
        (voter principal)
    )
    (map-get? proposal-votes {
        proposal-id: proposal-id,
        voter: voter,
    })
)

;; Read-only function to get curator profile information
(define-read-only (get-curator-profile (curator principal))
    (default-to {
        reputation-score: u0,
        total-proposals: u0,
        successful-proposals: u0,
        total-votes-cast: u0,
        community-tier: u1,
        last-active: u0,
    }
        (map-get? curator-profiles { curator: curator })
    )
)

;; Read-only function to get curated collection details
(define-read-only (get-curated-collection (collection-id uint))
    (map-get? curated-collections { collection-id: collection-id })
)

;; Read-only function to get community event details
(define-read-only (get-community-event (event-id uint))
    (map-get? community-events { event-id: event-id })
)

;; Read-only function to get event participation details
(define-read-only (get-event-participation
        (event-id uint)
        (participant principal)
    )
    (map-get? event-participants {
        event-id: event-id,
        participant: participant,
    })
)

;; Read-only function to check if a voter is eligible to vote on a proposal
(define-read-only (check-voting-eligibility
        (voter principal)
        (artist principal)
    )
    (>= (calculate-voting-power voter artist) MIN-VOTING-POWER)
)

;; Read-only function to get voting power for a voter-artist pair
(define-read-only (get-voting-power
        (voter principal)
        (artist principal)
    )
    (calculate-voting-power voter artist)
)

;; Read-only function to get the current proposal ID counter
(define-read-only (get-next-proposal-id)
    (var-get next-proposal-id)
)

;; Read-only function to get the current collection ID counter
(define-read-only (get-next-collection-id)
    (var-get next-collection-id)
)

;; Read-only function to get the current event ID counter
(define-read-only (get-next-event-id)
    (var-get next-event-id)
)
