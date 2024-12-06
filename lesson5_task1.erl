-module(kv_store_benchmark).
-export([run/0, benchmark/1, create_table/1, add_element/2, update_element/2, delete_element/2, get_element/2]).

%% Запустити тест
run() ->
    Mechanisms = [list, proplist, map, ets, gb_trees],
    Results = lists:map(fun benchmark/1, Mechanisms),
    io:format("~nРезультати тестів:~n~p~n", [Results]).

%% Бенчмарк для кожного механізму
benchmark(Mechanism) ->
    Table = create_table(Mechanism),
    AddTime = timer:tc(fun() -> add_element(Mechanism, Table) end),
    UpdateTime = timer:tc(fun() -> update_element(Mechanism, Table) end),
    DeleteTime = timer:tc(fun() -> delete_element(Mechanism, Table) end),
    ReadTime = timer:tc(fun() -> get_element(Mechanism, Table) end),
    {Mechanism, AddTime, UpdateTime, DeleteTime, ReadTime}.

%% Ініціалізація таблиці
create_table(list) -> [];
create_table(proplist) -> [];
create_table(map) -> #{};
create_table(ets) -> ets:new(benchmark_table, [set, public]);
create_table(gb_trees) -> gb_trees:empty().

%% Додавання елементів
add_element(list, Table) -> [{key, value} | Table];
add_element(proplist, Table) -> [{key, value} | Table];
add_element(map, Table) -> maps:put(key, value, Table);
add_element(ets, Table) -> ets:insert(Table, {key, value}), Table;
add_element(gb_trees, Table) -> gb_trees:insert(key, value, Table).

%% Оновлення елементів
update_element(list, Table) -> lists:keyreplace(key, 1, Table, {key, new_value});
update_element(proplist, Table) -> lists:keyreplace(key, 1, Table, {key, new_value});
update_element(map, Table) -> maps:put(key, new_value, Table);
update_element(ets, Table) -> ets:insert(Table, {key, new_value}), Table;
update_element(gb_trees, Table) -> gb_trees:insert(key, new_value, Table).

%% Видалення елементів
delete_element(list, Table) -> lists:keydelete(key, 1, Table);
delete_element(proplist, Table) -> lists:keydelete(key, 1, Table);
delete_element(map, Table) -> maps:remove(key, Table);
delete_element(ets, Table) -> ets:delete(Table, key), Table;
delete_element(gb_trees, Table) -> gb_trees:delete(key, Table).

%% Читання елемента
get_element(list, Table) -> lists:keyfind(key, 1, Table);
get_element(proplist, Table) -> lists:keyfind(key, 1, Table);
get_element(map, Table) -> maps:get(key, Table);
get_element(ets, Table) -> case ets:lookup(Table, key) of [] -> undefined; [{_, Value}] -> Value end;
get_element(gb_trees, Table) -> case gb_trees:lookup(key, Table) of none -> undefined; {value, Value} -> Value end.
