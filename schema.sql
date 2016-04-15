create table karma (
    member text not null,
    action text not null,
    stamp timestamp with time zone default current_timestamp
);

create index karma_stamp_idx on karma (stamp);