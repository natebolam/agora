create table if not exists period_metas (
       id                     INTEGER    PRIMARY KEY,
       type                   INTEGER    not null,
       votes_cast             INTEGER    not null,
       votes_available        INTEGER    not null,
       voters_num             INTEGER    not null,
       total_voters_num       INTEGER    not null,
       quorum                 INTEGER    not null,
       when_started           TIMESTAMP  with time zone not null,
       start_level            INTEGER    not null,
       end_level              INTEGER    not null,
       last_block_level       INTEGER    not null,
       last_block_hash        BYTEA      not null,
       prev_block_hash        BYTEA      not null,
       ballots_yay            INTEGER    not null,
       ballots_nay            INTEGER    not null,
       ballots_pass           INTEGER    not null
);

create table if not exists voters (
       pbk_hash               BYTEA      not null,
       name                   TEXT,
       logo_url               TEXT,
       profile_url            TEXT,
       rolls                  INTEGER    not null,
       period__id             INTEGER    not null,

       foreign key (period__id) references period_metas (id),
       primary key (pbk_hash)
);

create table if not exists proposals (
       id                     BIGSERIAL  PRIMARY KEY,
       period__id             INTEGER    not null,
       hash                   BYTEA      not null,
       time_proposed          TIMESTAMP  with time zone not null,
       proposer__pbk_hash     BYTEA      not null,
       votes_cast             INTEGER    not null,
       voters_num             INTEGER    not null,

       discourse_title        TEXT,
       discourse_short_desc   TEXT,
       discourse_long_desc    TEXT,
       discourse_file         TEXT,
       discourse_topic_id     INTEGER,
       discourse_post_id      INTEGER,

       foreign key (period__id)          references period_metas (id),
       foreign key (proposer__pbk_hash)  references voters (pbk_hash)
);

create index if not exists proposal_period_id on proposals (period__id);
create index if not exists proposal_proposer_hash on proposals (proposer__pbk_hash);

create table if not exists block_metas (
        level                  INTEGER    PRIMARY KEY,
        hash                   BYTEA      not null,
        predecessor            BYTEA      not null,
        block_time             TIMESTAMP  with time zone not null,
        voting_period_type     INTEGER    not null
);

create index if not exists block_meta_hash on block_metas (hash);
create index if not exists block_meta_predecessor on block_metas (predecessor);

create table if not exists proposal_votes (
       id                     BIGSERIAL  PRIMARY KEY,
       voter__pbk_hash        BYTEA      not null,
       proposal__id           BIGINT     not null,
       casted_rolls           INTEGER    not null,
       operation              BYTEA      not null,
       vote_time              TIMESTAMP  with time zone  not null,
       block__level           INTEGER    not null,

       foreign key (voter__pbk_hash)     references voters (pbk_hash),
       foreign key (proposal__id)        references proposals (id),
       foreign key (block__level)        references block_metas (level),
       unique (voter__pbk_hash, proposal__id)
);

create index if not exists proposal_vote_voter_pbk_hash on proposal_votes (voter__pbk_hash);
create index if not exists proposal_vote_proposal_id on proposal_votes (proposal__id);
create index if not exists proposal_vote_operation on proposal_votes (operation);

create table if not exists ballots (
       id                     BIGSERIAL  PRIMARY KEY,
       vote_type              SMALLINT   not null,
       voter__pbk_hash        BYTEA      not null,
       proposal__id           BIGINT     not null,
       period__id             INTEGER    not null,
       casted_rolls           INTEGER    not null,
       operation              BYTEA      not null,
       ballot_time            TIMESTAMP  with time zone not null,
       ballot_decision        INTEGER    not null,
       block__level           INTEGER    not null,

       foreign key (voter__pbk_hash)     references voters (pbk_hash),
       foreign key (proposal__id)        references proposals (id),
       foreign key (period__id)          references period_metas (id),
       foreign key (block__level)        references block_metas (level),
       unique (vote_type, voter__pbk_hash, proposal__id)
);

create index if not exists ballot_voter_pbk_hash on ballots (voter__pbk_hash);
create index if not exists ballot_proposal_id on ballots (proposal__id);
create index if not exists ballot_vote_type on ballots (vote_type);
create index if not exists ballot_operation on ballots (operation);

create table if not exists council (
       pbk_hash               BYTEA      not null,
       stage                  INTEGER    not null,

       primary key (pbk_hash)
);

create table if not exists stkr_proposals (
       id                     BIGSERIAL  PRIMARY KEY,
       stage                  INTEGER    not null,
       hash                   BYTEA      not null,
       proposer__pbk_hash     BYTEA      not null,
       voters_num             INTEGER    not null,

       discourse_title        TEXT,
       discourse_short_desc   TEXT,
       discourse_long_desc    TEXT,
       discourse_file         TEXT,
       discourse_topic_id     INTEGER,
       discourse_post_id      INTEGER,

       foreign key (proposer__pbk_hash)  references council (pbk_hash)
);

create index if not exists stkr_proposal_stage on stkr_proposals (stage);
create index if not exists stkr_proposal_hash on stkr_proposals (proposer__pbk_hash);

create table if not exists policy (
       hash                   BYTEA      not null,
       url                    TEXT,

       primary key (hash)
);

create table if not exists votes (
       id                     BIGSERIAL  PRIMARY KEY,
       voter__pbk_hash        BYTEA      not null,
       proposal__id           BIGINT     not null,
       block__level           INTEGER    not null,

       foreign key (voter__pbk_hash)     references council (pbk_hash),
       foreign key (proposal__id)        references stkr_proposals (id),
       foreign key (block__level)        references block_metas (level),
       unique (voter__pbk_hash, proposal__id)
);

create index if not exists vote_voter_pbk_hash on votes (voter__pbk_hash);
create index if not exists vote_proposal_id on votes (proposal__id);
