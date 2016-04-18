create table karma (
    member text not null,
    action text not null,
    stamp timestamp with time zone default current_timestamp
);

create index karma_stamp_idx on karma (stamp);

create sequence karma_seq;

alter table karma add column action_id integer unique not null default nextval('karma_seq');
