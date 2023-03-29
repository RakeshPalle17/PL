%-*- mode: prolog; -*-


% An order-item is represented using the structure
% orderItem(SKU, Category, NUnits, UnitPrice).

%% Test data
item1(order_item(cw123, cookware, 3, 12.50)).
item2(order_item(cw126, cookware, 2, 11.50)).
item3(order_item(ap723, apparel, 2, 10.50)).
item4(order_item(cw127, cookware, 1, 9.99)).
item5(order_item(ap273, apparel, 3, 21.50)).
item6(order_item(fd825, food, 1, 2.48)).

order_items([ Item1, Item2, Item3, Item4, Item5, Item6 ]) :-
    item1(Item1),
    item2(Item2),
    item3(Item3),
    item4(Item4),
    item5(Item5),
    item6(Item6).
	     
cookware_items([Item1, Item2, Item4]) :-
    item1(Item1),
    item2(Item2),
    item4(Item4).

apparel_items([Item3, Item5]) :-
    item3(Item3),
    item5(Item5).

food_items([Item6]) :-
    item6(Item6).

  
% #1: 15-points
% items_total1(Items, Total): given a list Items of order-item, match
% Total with the total for the order, i.e. sum n-units*unit-price over
% all items.
% Restriction: must be recursive, but may not define any auxiliary procedures.
%items_total1(_Items, _Total) :-

items_total1([], 0).
items_total1([order_item(_, _, Units, Unit_Price) | Tail], Total) :-
        items_total1(Tail, TotalOfTail),
        Total is Units * Unit_Price + TotalOfTail.   

:-begin_tests(items_total1).
test(empty) :-
    items_total1([], 0).
test(single) :-
    items_total1([order_item(_, _, 3.0, 7.0)], 21.0).
test(seq_simple) :-
    Items = [
	order_item(_, _, 1, 1), 
	order_item(_, _, 1, 2), 
	order_item(_, _, 1, 3), 
	order_item(_, _, 1, 4), 
	order_item(_, _, 1, 5)
    ],
    items_total1(Items, 15).
test(all) :-
    order_items(Items),
    Total is 3*12.5 + 2*11.5 + 2*10.5 + 1*9.99 + 3*21.5 + 1*2.48,
    items_total1(Items, Total).
:-end_tests(items_total1).


% #2: 15-points
% same specs as items_total1/2.
% Restriction: must be implemented tail-recursive (i.e. any
% recursive call must be the last call in the rule).
%
% Hint: Implement as a wrapper around an auxiliary procedure items_total2/3
% which uses an accumulator.

items_total2(Items, Total) :- 
    items_total2(Items, 0, Total).

items_total2([], Acc, Acc).

items_total2([order_item(_, _, Units, Unit_Price) | Rest], Acc, Total) :-
    Acc1 is Acc + Units * Unit_Price,
    items_total2(Rest, Acc1, Total).

:-begin_tests(items_total2).
test(empty) :-
    items_total2([], 0).
test(single) :-
    items_total2([order_item(_, _, 3.0, 7.0)], 21.0).
test(seq_simple) :-
    Items = [
	order_item(_, _, 1, 1), 
	order_item(_, _, 1, 2), 
	order_item(_, _, 1, 3), 
	order_item(_, _, 1, 4), 
	order_item(_, _, 1, 5)
    ],
    items_total2(Items, 15).
test(all) :-
    order_items(Items),
    Total is 3*12.5 + 2*11.5 + 2*10.5 + 1*9.99 + 3*21.5 + 1*2.48,
    items_total2(Items, Total).
:-end_tests(items_total2).

% #3: 15-points
% items_with_category(Items, Category, CategoryItems): CategoryItems
% matches those order-items in Items having category Category.
% Restriction: Must be implemented using recursion.
%
% Hint: X \= Y succeeds iff X = Y fails.
%items_with_category(_Items, _Category, _CategoryItems) :-

items_with_category([], _, []).
items_with_category([order_item(SKU, Category, NUnits, UnitPrice)|Tail], Category, [order_item(SKU, Category, NUnits, UnitPrice)|CategoryTail]) :-
    items_with_category(Tail, Category, CategoryTail).

items_with_category([order_item(_, OtherCategory, _, _)|Tail], Category, CategoryTail) :-
    OtherCategory \= Category,
    items_with_category(Tail, Category, CategoryTail).

:-begin_tests(items_with_category).
test(cookware, nondet) :-
    order_items(Items),
    items_with_category(Items, cookware, CookwareItems),
    cookware_items(CookwareItems).
test(apparel, nondet) :-
    order_items(Items),
    items_with_category(Items, apparel, ApparelItems),
    apparel_items(ApparelItems).
test(food, nondet) :-
    order_items(Items),
    items_with_category(Items, food, FoodItems),
    food_items(FoodItems).
test(unknown, nondet) :-
    order_items(Items),
    items_with_category(Items, unknown, []).
:-end_tests(items_with_category).


% #4: 15-points
% expensive_item_skus(Items, Price, ExpensiveSKUs): Match ExpensiveSKUs
% with the SKUs of those order-items in Items having unit-price > Price.

expensive_item_skus(Items, Price, ExpensiveSKUs) :-
    findall(SKU, (member(order_item(SKU, _, _, UnitPrice), Items), UnitPrice > Price), ExpensiveSKUs).


:-begin_tests(expensive_item_skus).
test(gt20, nondet) :-
    order_items(Items),
    expensive_item_skus(Items, 20, [ap273]).
test(gt12, nondet) :-
    order_items(Items),
    expensive_item_skus(Items, 12, [cw123, ap273]).
test(gt22, nondet) :-
    order_items(Items),
    expensive_item_skus(Items, 22, []).
test(all, nondet) :-
    order_items(Items),
    expensive_item_skus(Items, 0, [cw123, cw126, ap723, cw127, ap273, fd825]).
:-end_tests(expensive_item_skus).


% #5: 15-points
% expensive_item_sku(Items, Price, SKU): SKU matches the sku of an
% order-item in Items having unit-price > price.
% Restriction: must be defined using a single rule, cannot use recursion.
%
% Hint: use member/2
expensive_item_sku(Items, Price, ExpensiveSKU) :- 
    member(order_item(ExpensiveSKU, _, _, Unit_Price), Items),
    Unit_Price > Price.

:-begin_tests(expensive_item_sku).
test(gt20, all(Z = [ap273])) :-
    order_items(Items),
    expensive_item_sku(Items, 20, Z).
test(gt12, all(Z = [cw123, ap273])) :-
    order_items(Items),
    expensive_item_sku(Items, 12, Z).
test(gt22, fail) :-
    order_items(Items),
    expensive_item_sku(Items, 22, _Z).
test(all, all(Z = [cw123, cw126, ap723, cw127, ap273, fd825])) :-
    order_items(Items),
    expensive_item_sku(Items, 0, Z).
:-end_tests(expensive_item_sku).


% #6: 25-points
% Prolog's '+' operator is left-associative, as shown by the
% following REPL log:

% The paren below are redundant as shown in the output
% ?- X = (1 + 2) + 3.
% X = 1+2+3.
%
% The paren below are not redundant as shown in the output
% ?- X = 1 + (2 + 3).
% X = 1+(2+3).

% A Prolog atomic is either an integer or atom.
% ?- atomic(22).
% true.
% ?- atomic(a).
% true.
% ?- atomic([]).
% true.
% ?- atomic(f(2)).
% false.

% An unrestricted plus expression is either an atomic or of
% the form A + B where A and B are unrestricted plus expressions.

% A left plus expression is either an atomic or of the form
% A + B where A is a left plus expression and B is an atomic.

% left_plus(PlusExpr, LeftPlusExpr): Given an unrestricted plus expression
% PlusExpr, match LeftPlusExpr to the corresponding left plus expression.
%
% Hint: Recurse left-to-right into PlusExpr accumulating atomics into
% a left plus expression accumulator.  Initialize accumulator to an
% invalid plus expression and special-case that initial accumulator
% % in the base cases.

left_plus([], []).

left_plus(X, X) :-
    atomic(X).

left_plus(X + Y, _LeftPlus) :-
    left_plus(X, _XPlus),
    left_plus(Y, _YPlus).

:- begin_tests(left_plus).
test(int, nondet) :-
    left_plus(1, 1).
test(atom, nondet) :-
    left_plus(a, a).
test(nil, nondet) :-
    left_plus([], []).
test(simple, nondet) :-
    left_plus(1 + 2, 1 + 2).
test(left, nondet) :-
    X = 1 + 2 + a + b + 4,
    left_plus(X, X).
test(right, nondet) :-
    Right = 1 + (2 + (3 + (4 + 5))),
    Left = 1 + 2 + 3 + 4 + 5,
    left_plus(Right, Left).
test(mixed, nondet) :-
    Mixed = 1 + (2 + 3) + ((4 + 5) + 6),
    Left = 1 + 2 + 3 + 4 + 5 + 6,
    left_plus(Mixed, Left).
test(complex, nondet) :-
    Complex = 1 + (2 + (3 + 4)) + ((5 + 6) + (7 + 8 + (9 + 0))),
    Left = 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 0,
    left_plus(Complex, Left).
:- end_tests(left_plus).
