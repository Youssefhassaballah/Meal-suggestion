% Ingredients database
ingredient(bread).
ingredient(eggs).
ingredient(milk).
ingredient(cheese).
ingredient(tomato).
ingredient(onion).
ingredient(garlic).
ingredient(chicken).
ingredient(beef).
ingredient(pasta).
ingredient(rice).
ingredient(potato).
ingredient(carrot).
ingredient(oats).
ingredient(banana).
ingredient(apple).
ingredient(honey).
ingredient(olive_oil).
ingredient(salt).
ingredient(pepper).
ingredient(spinach).
ingredient(bell_pepper).
ingredient(mushroom).
ingredient(cream).
ingredient(bacon).
ingredient(butter).
ingredient(corn).
ingredient(cucumber).
ingredient(lemon).
ingredient(orange).
ingredient(strawberry).
ingredient(peanut_butter).
ingredient(beans).
ingredient(lentils).
ingredient(eggplant).
ingredient(zucchini).
ingredient(broccoli).
ingredient(cauliflower).
ingredient(coconut_milk).
ingredient(chili).
ingredient(chocolate).
ingredient(sugar).
ingredient(vanilla).
ingredient(flour).
ingredient(tortilla).  % Added missing ingredient used in egg_bacon_wrap
ingredient(yogurt).    % Added missing ingredient used in fruit_parfait

% Meal database organized by type
% breakfast
meal(omelette, [eggs, cheese, salt, pepper], breakfast).
meal(french_toast, [bread, eggs, milk, sugar], breakfast).
meal(banana_smoothie, [banana, milk, honey], breakfast).
meal(egg_sandwich, [bread, eggs, cheese], breakfast).
meal(oat_porridge, [oats, milk, honey], breakfast).
meal(apple_pancakes, [apple, flour, eggs, milk], breakfast).
meal(veggie_omelette, [eggs, tomato, onion, pepper], breakfast).
meal(honey_toast, [bread, honey, butter], breakfast).
meal(banana_omelette, [banana, eggs, olive_oil], breakfast).
meal(milk_eggs, [milk, eggs], breakfast).
meal(bread_milk, [bread, milk], breakfast).
meal(cheese_bread, [bread, cheese], breakfast).
meal(spinach_omelette, [eggs, spinach, cheese], breakfast).
meal(strawberry_oatmeal, [oats, milk, strawberry], breakfast).
meal(peanut_butter_toast, [bread, peanut_butter, banana], breakfast).
meal(egg_bacon_wrap, [eggs, bacon, tortilla], breakfast).
meal(sweet_potato_hash, [potato, olive_oil, pepper], breakfast).
meal(pancake_stack, [flour, eggs, milk, butter], breakfast).
meal(broccoli_eggs, [broccoli, eggs, olive_oil], breakfast).
meal(mushroom_scramble, [mushroom, eggs, garlic], breakfast).

% dessert
meal(chocolate_cake, [flour, sugar, eggs, butter, chocolate], dessert).
meal(strawberry_cream, [strawberry, cream, sugar], dessert).
meal(banana_pudding, [banana, milk, vanilla, sugar], dessert).
meal(honey_apple_crisp, [apple, honey, oats, butter], dessert).
meal(chocolate_brownies, [chocolate, flour, butter, eggs, sugar], dessert).
meal(vanilla_pudding, [milk, sugar, vanilla, eggs], dessert).
meal(peanut_butter_bars, [peanut_butter, oats, honey], dessert).
meal(lemon_tart, [flour, butter, lemon, sugar], dessert).
meal(coconut_balls, [coconut_milk, sugar, vanilla], dessert).
meal(fruit_parfait, [yogurt, strawberry, banana, honey], dessert).

% snack
meal(fruit_salad, [banana, apple, honey], snack).
meal(oat_bar, [oats, honey, milk], snack).
meal(fried_eggs, [eggs, olive_oil, salt], snack).
meal(milkshake, [milk, banana, honey], snack).
meal(bread_butter, [bread, butter], snack).
meal(salted_potato, [potato, salt], snack).
meal(cheese_bites, [cheese, bread, olive_oil], snack).
meal(chocolate_milk, [milk, honey], snack).
meal(apple_slices, [apple, honey], snack).
meal(potato_chips, [potato, salt, olive_oil], snack).
meal(grilled_tomato, [tomato, olive_oil], snack).
meal(honey_eggs, [eggs, honey], snack).
meal(orange_slices, [orange, honey], snack).
meal(peanut_butter_apple, [apple, peanut_butter], snack).
meal(spicy_chips, [potato, chili, salt], snack).
meal(broccoli_bites, [broccoli, olive_oil], snack).
meal(cream_cheese_dip, [cream, cheese, garlic], snack).
meal(strawberry_cups, [strawberry, cream], snack).
meal(corn_fritters, [corn, eggs, flour], snack).
meal(banana_chips, [banana, olive_oil], snack).

% lunch
meal(chicken_rice, [chicken, rice, olive_oil, salt], lunch).
meal(beef_pasta, [beef, pasta, tomato, garlic], lunch).
meal(vegetable_soup, [carrot, onion, garlic, olive_oil], lunch).
meal(tomato_rice, [tomato, rice, olive_oil, salt], lunch).
meal(grilled_chicken, [chicken, olive_oil, pepper], lunch).
meal(beef_sandwich, [bread, beef, tomato, onion], lunch).
meal(chicken_pasta, [chicken, pasta, olive_oil, garlic], lunch).
meal(mixed_salad, [tomato, onion, olive_oil, salt], lunch).
meal(banana_rice, [banana, rice, honey], lunch).
meal(potato_soup, [potato, onion, garlic], lunch).
meal(oat_chicken, [oats, chicken, olive_oil], lunch).
meal(beef_rice, [beef, rice, pepper], lunch).
meal(eggplant_stew, [eggplant, tomato, onion, olive_oil], lunch).
meal(lentil_soup, [lentils, onion, garlic, salt], lunch).
meal(cauliflower_rice, [cauliflower, rice, olive_oil], lunch).
meal(chili_beef, [beef, chili, tomato, garlic], lunch).
meal(zucchini_pasta, [zucchini, pasta, olive_oil], lunch).
meal(egg_fried_rice, [rice, eggs, onion, olive_oil], lunch).
meal(lemon_chicken, [chicken, lemon, olive_oil], lunch).
meal(coconut_curry, [chicken, coconut_milk, chili], lunch).

% dinner
meal(grilled_cheese, [bread, cheese, butter], dinner).
meal(stir_fry, [chicken, onion, garlic, olive_oil], dinner).
meal(potato_wedges, [potato, olive_oil, salt, pepper], dinner).
meal(pasta_salad, [pasta, tomato, olive_oil, salt], dinner).
meal(egg_burger, [bread, eggs, pepper], dinner).
meal(chicken_salad, [chicken, tomato, olive_oil], dinner).
meal(beef_casserole, [beef, onion, garlic], dinner).
meal(cheese_rice, [cheese, rice, olive_oil], dinner).
meal(tomato_pasta, [pasta, tomato, garlic], dinner).
meal(fried_potato, [potato, olive_oil, salt], dinner).
meal(carrot_stew, [carrot, onion, garlic], dinner).
meal(honey_rice, [rice, honey, butter], dinner).
meal(broccoli_casserole, [broccoli, cheese, milk], dinner).
meal(spicy_eggplant, [eggplant, chili, garlic, olive_oil], dinner).
meal(corn_rice, [corn, rice, butter], dinner).
meal(cucumber_salad, [cucumber, olive_oil, lemon], dinner).
meal(mushroom_risotto, [mushroom, rice, cream], dinner).
meal(bell_pepper_stir_fry, [bell_pepper, chicken, garlic], dinner).
meal(sweet_and_sour_chicken, [chicken, honey, lemon], dinner).
meal(chicken_lentil_soup, [chicken, lentils, onion], dinner).

% Helper predicates
subset([], _).
subset([H|T], List) :- member(H, List), subset(T, List).

% 1. Suggest a meal based on available ingredients
suggest_meal(Ingredients, Meal, Type) :-
    meal(Meal, MealIngredients, Type),
    subset(MealIngredients, Ingredients).

% 2. Suggest a meal for a specific time (breakfast/lunch/dinner/snack)
suggest_meal_type(Ingredients, Type, Meal) :-
    meal(Meal, MealIngredients, Type),
    subset(MealIngredients, Ingredients).

% 3. Suggest a meal that avoids certain ingredients
avoid_ingredients(Meal, AvoidList) :-
    meal(Meal, Ingredients, _),
    \+ (member(X, Ingredients), member(X, AvoidList)).

% 4. Suggest a vegetarian meal
vegetarian_meal(Meal) :-
    meal(Meal, Ingredients, _),
    \+ member(chicken, Ingredients),
    \+ member(beef, Ingredients),
    \+ member(bacon, Ingredients).

% 5. Suggest a high protein meal
high_protein_meal(Meal) :-
     meal(Meal, Ingredients, _),
     (member(chicken, Ingredients); 
     member(eggs, Ingredients); 
     member(beef, Ingredients);
     member(lentils, Ingredients);
     member(beans, Ingredients)).

% 6. Suggest a high carb meal
high_carb_meal(Meal) :-
    meal(Meal, Ingredients, _),
    (member(rice, Ingredients); 
     member(pasta, Ingredients); 
     member(bread, Ingredients);
     member(potato, Ingredients);
     member(oats, Ingredients)).

% 7. Suggest a quick meal (3 ingredients or less)
quick_meal(Meal) :-
    meal(Meal, Ingredients, _),
    length(Ingredients, N), 
    N =< 3.

% 8. Suggest a sweet meal (dessert)
sweet_meal(Meal) :-
    meal(Meal, _, dessert).

% 9. Suggest a vegetarian meal containing only vegetables
veggie_meal(Meal) :-
    meal(Meal, Ingredients, _),
    \+ (member(X, Ingredients), 
        (member(X, [chicken, beef, bacon, eggs, milk, cheese, cream, yogurt]))).

% 10. Suggest a meal containing fruits
fruit_meal(Meal) :-
    meal(Meal, Ingredients, _),
    (member(banana, Ingredients); 
     member(apple, Ingredients); 
     member(strawberry, Ingredients); 
     member(orange, Ingredients);
     member(lemon, Ingredients)).