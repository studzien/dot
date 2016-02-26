-record(dot_process, {id :: pid() | atom() | reference(),
                      pid :: pid(),
                      ancestors :: [pid() | atom()]}).
