create table if not exists block_metas (
        level                  INTEGER    PRIMARY KEY,
        hash                   BYTEA      not null,
        predecessor            BYTEA      not null,
        block_time             TIMESTAMP  with time zone not null
);

create index if not exists block_meta_hash on block_metas (hash);
create index if not exists block_meta_predecessor on block_metas (predecessor);

create table if not exists council (
       pbk_hash               BYTEA      not null,
       stage                  INTEGER    not null,

       primary key (pbk_hash, stage)
);

create table if not exists stkr_proposals (
       id                     INTEGER    not null,
       stage                  INTEGER    not null,
       epoch                 INTEGER    not null,
       hash                   BYTEA      not null,
       time_proposed          TIMESTAMP  with time zone not null,
       description            TEXT,

       discourse_title        TEXT,
       discourse_short_desc   TEXT,
       discourse_long_desc    TEXT,
       discourse_topic_id     INTEGER,
       discourse_post_id      INTEGER,

       primary key (id, epoch)
);

create table if not exists votes (
       seq                    BIGSERIAL,
       stage                  INTEGER    not null,
       epoch                  INTEGER    not null,
       voter_pbk_hash         BYTEA      not null,
       proposal_number        INTEGER    not null,
       vote_time              TIMESTAMP  with time zone  not null,

       primary key (epoch, stage, voter_pbk_hash, proposal_number),
       foreign key (proposal_number, epoch)   references stkr_proposals (id, epoch)
);

create table if not exists policy (
       proposal_id            INTEGER    not null,
       epoch                 INTEGER    not null,
       hash                   BYTEA      not null,
       description            TEXT,
       url                    TEXT,

       primary key (proposal_id, epoch, hash),
       foreign key (proposal_id, epoch)     references stkr_proposals (id, epoch)
);
