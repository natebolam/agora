create table if not exists period_metas (
       id                     INTEGER    PRIMARY KEY,
       type                   INTEGER    not null,
       votes_cast             INTEGER    not null,
       votes_available        INTEGER    not null,
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
       rolls                  INTEGER    not null,

       primary key (pbk_hash)
);

create table if not exists proposals (
       id                     BIGSERIAL  PRIMARY KEY,
       period__id             INTEGER    not null,
       hash                   BYTEA      not null,
       time_proposed          TIMESTAMP  with time zone not null,
       proposer__pbk_hash     BYTEA      not null,

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

create table if not exists proposal_votes (
       id                     BIGSERIAL  PRIMARY KEY,
       voter__pbk_hash        BYTEA      not null,
       proposal__id           BIGINT     not null,
       casted_rolls           INTEGER    not null,
       operation              BYTEA      not null,
       vote_time              TIMESTAMP  with time zone  not null,

       foreign key (voter__pbk_hash)     references voters (pbk_hash),
       foreign key (proposal__id)        references proposals (id),
       unique (voter__pbk_hash, proposal__id)
);

create index if not exists proposal_vote_voter_pbk_hash on proposal_votes (voter__pbk_hash);
create index if not exists proposal_vote_proposal_id on proposal_votes (proposal__id);

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

       foreign key (voter__pbk_hash)     references voters (pbk_hash),
       foreign key (proposal__id)        references proposals (id),
       foreign key (period__id)          references period_metas (id),
       unique (vote_type, voter__pbk_hash, proposal__id)
);

create index if not exists ballot_voter_pbk_hash on ballots (voter__pbk_hash);
create index if not exists ballot_proposal_id on ballots (proposal__id);
create index if not exists ballot_vote_type on ballots (vote_type);
