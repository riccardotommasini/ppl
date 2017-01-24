-module(tree).
-compile(export_all).

%BINARY TREES (Recursive)


%Nodes are tuple of the form {key, 'value', [branches]+}
% values can be 'nil'
% branches are optional 
empty() -> {node,'nil'}.


insert(Key, Val, {node,'nil'}) -> 
	{node, {Key, Val, {node, 'nil'}, {node, 'nil'}}};
insert(NewKey, NewVal, {node, {Key, Val, Left, Right}}) when NewKey < Key -> 
	{node, {Key, Val, insert(NewKey, NewVal, Left), Right}};
insert(NewKey, NewVal, {node, {Key, Val, Left, Right}}) when NewKey > Key -> 
	{node, {Key, Val, Left, insert(NewKey, NewVal, Right)}};
insert(Key, Val, {node, {Key, _, Left, Right}}) -> 
	{node, {Key, Val, Left, Right}}.


lookup(_, {node, 'nil'}) ->
	undefined;
lookup(Key, {node, {Key, Val, _, _}}) ->
	{ok, Val};
lookup(Key, {node, {NodeKey, _, Left, _}}) when Key < NodeKey ->
	lookup(Key, Left);
lookup(Key, {node, {_, _, _, Right}}) -> %all the other cases
	lookup(Key, Right).
