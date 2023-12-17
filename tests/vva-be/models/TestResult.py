from typing import TypedDict


class Metrics(TypedDict):
    outcome: str
    start_date: int
    end_date: int
    commit_hash: str
    build_id: int
    test_name: str
