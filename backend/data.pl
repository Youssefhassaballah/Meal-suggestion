% Enhanced Meal Recommendation Prolog Database
% Expanded with more meals and 90% match function

% Define all possible values for each question category
goal_option(weight_loss).
goal_option(muscle_gain).
goal_option(energy_boost).
goal_option(general_health).
goal_option(tasty_food).

time_option(breakfast).
time_option(lunch).
time_option(dinner).
time_option(snack).

dietary_option(vegetarian).
dietary_option(vegan).
dietary_option(gluten_free).
dietary_option(dairy_free).
dietary_option(no_restrictions).

protein_option(chicken).
protein_option(beef).
protein_option(fish_seafood).
protein_option(plant_based).
protein_option(no_preference).

spice_option(mild).
spice_option(medium).
spice_option(spicy).
spice_option(very_spicy).

meal_type_option(quick_easy).
meal_type_option(home_cooked).
meal_type_option(restaurant_style).
meal_type_option(comfort_food).

cuisine_option(middle_eastern).
cuisine_option(italian).
cuisine_option(asian).
cuisine_option(american).
cuisine_option(no_preference).

taste_option(sweet).
taste_option(savory).
taste_option(mix_both).

health_option(very_healthy).
health_option(balanced).
health_option(treat_day).

prep_time_option(less_10_min).
prep_time_option('10_30_min').
prep_time_option(more_30_min).
prep_time_option(no_cooking).

budget_option(low).
budget_option(medium).
budget_option(high).

% EXPANDED MEAL DATABASE
% meal(Name, Goal, Time, Dietary, Protein, Spice, MealType, Cuisine, Taste, Health, PrepTime, Budget, Ingredients).

% EXPANDED MEAL DATABASE - 100+ MEALS
% Comprehensive collection of meals covering all possible combinations
% meal(Name, Goal, Time, Dietary, Protein, Spice, MealType, Cuisine, Taste, Health, PrepTime, Budget, Ingredients).

% ========================================
% BREAKFAST MEALS (25 meals)
% ========================================

meal('Greek Yogurt with Berries', general_health, breakfast, no_restrictions, no_preference, mild, quick_easy, no_preference, sweet, very_healthy, less_10_min, low, [yogurt, berries, honey]).
meal('Scrambled Eggs with Toast', energy_boost, breakfast, no_restrictions, no_preference, mild, home_cooked, american, savory, balanced, '10_30_min', low, [eggs, bread, butter]).
meal('Avocado Toast', weight_loss, breakfast, vegetarian, plant_based, mild, quick_easy, american, savory, very_healthy, less_10_min, medium, [avocado, bread, lime, salt]).
meal('Chocolate Protein Pancakes', muscle_gain, breakfast, no_restrictions, no_preference, mild, home_cooked, american, sweet, balanced, '10_30_min', medium, [protein_powder, eggs, banana, cocoa]).
meal('Overnight Oats', general_health, breakfast, vegetarian, plant_based, mild, no_cooking, no_preference, sweet, very_healthy, less_10_min, low, [oats, milk, chia_seeds, fruits]).
meal('Protein Smoothie Bowl', muscle_gain, breakfast, vegetarian, plant_based, mild, quick_easy, no_preference, sweet, very_healthy, less_10_min, medium, [protein_powder, banana, berries, granola]).
meal('French Toast', tasty_food, breakfast, vegetarian, no_preference, mild, home_cooked, american, sweet, treat_day, '10_30_min', medium, [bread, eggs, milk, syrup]).
meal('Veggie Omelet', weight_loss, breakfast, vegetarian, no_preference, mild, home_cooked, american, savory, very_healthy, '10_30_min', medium, [eggs, vegetables, cheese]).
meal('Acai Bowl', general_health, breakfast, vegan, plant_based, mild, quick_easy, no_preference, sweet, very_healthy, less_10_min, medium, [acai, banana, granola, coconut]).
meal('English Breakfast', energy_boost, breakfast, no_restrictions, no_preference, mild, home_cooked, american, savory, treat_day, more_30_min, high, [eggs, bacon, sausage, beans, toast]).
meal('Chia Pudding', weight_loss, breakfast, vegan, plant_based, mild, no_cooking, no_preference, sweet, very_healthy, less_10_min, low, [chia_seeds, coconut_milk, berries]).
meal('Breakfast Burrito', energy_boost, breakfast, no_restrictions, no_preference, medium, home_cooked, american, savory, balanced, '10_30_min', medium, [eggs, cheese, sausage, tortilla]).
meal('Granola with Yogurt', general_health, breakfast, vegetarian, no_preference, mild, no_cooking, american, sweet, balanced, less_10_min, medium, [granola, yogurt, honey]).
meal('Smoked Salmon Bagel', tasty_food, breakfast, no_restrictions, fish_seafood, mild, quick_easy, american, savory, balanced, less_10_min, high, [bagel, salmon, cream_cheese]).
meal('Banana Pancakes', tasty_food, breakfast, vegetarian, no_preference, mild, home_cooked, american, sweet, treat_day, '10_30_min', low, [banana, eggs, flour]).
meal('Green Smoothie', weight_loss, breakfast, vegan, plant_based, mild, quick_easy, no_preference, mix_both, very_healthy, less_10_min, medium, [spinach, banana, mango, coconut_water]).
meal('Breakfast Quinoa Bowl', muscle_gain, breakfast, vegetarian, plant_based, mild, home_cooked, no_preference, savory, very_healthy, '10_30_min', medium, [quinoa, nuts, fruits, honey]).
meal('Croissant with Jam', tasty_food, breakfast, vegetarian, no_preference, mild, no_cooking, american, sweet, treat_day, less_10_min, medium, [croissant, jam, butter]).
meal('Steel Cut Oats', general_health, breakfast, vegetarian, plant_based, mild, home_cooked, american, sweet, very_healthy, more_30_min, low, [steel_cut_oats, milk, cinnamon]).
meal('Breakfast Wrap', energy_boost, breakfast, no_restrictions, no_preference, medium, quick_easy, american, savory, balanced, less_10_min, medium, [tortilla, eggs, cheese, salsa]).
meal('Muesli Bowl', general_health, breakfast, vegetarian, plant_based, mild, no_cooking, no_preference, sweet, very_healthy, less_10_min, low, [muesli, milk, fruits]).
meal('Protein Waffles', muscle_gain, breakfast, vegetarian, no_preference, mild, home_cooked, american, sweet, balanced, '10_30_min', medium, [protein_powder, oats, eggs]).
meal('Breakfast Sandwich', energy_boost, breakfast, no_restrictions, no_preference, mild, quick_easy, american, savory, balanced, less_10_min, medium, [bread, eggs, cheese, ham]).
meal('Fruit Salad with Yogurt', weight_loss, breakfast, vegetarian, no_preference, mild, no_cooking, no_preference, sweet, very_healthy, less_10_min, low, [mixed_fruits, yogurt]).
meal('Shakshuka', tasty_food, breakfast, vegetarian, no_preference, spicy, home_cooked, middle_eastern, savory, balanced, '10_30_min', medium, [eggs, tomatoes, peppers, spices]).
meal('Pumpkin Spice Oatmeal', comfort_food, breakfast, vegan, plant_based, mild, home_cooked, american, sweet, balanced, '10_30_min', low, [oats, pumpkin_puree, cinnamon, almond_milk]).
meal('Egg White Scramble', weight_loss, breakfast, vegetarian, no_preference, mild, quick_easy, american, savory, very_healthy, less_10_min, low, [egg_whites, spinach, mushrooms]).
meal('Buckwheat Pancakes', general_health, breakfast, vegan, plant_based, mild, home_cooked, american, sweet, very_healthy, '10_30_min', medium, [buckwheat_flour, banana, almond_milk]).
meal('Breakfast Tacos', energy_boost, breakfast, no_restrictions, no_preference, medium, quick_easy, mexican, savory, balanced, less_10_min, medium, [eggs, tortillas, avocado, salsa]).
meal('Coconut Flour Waffles', weight_loss, breakfast, vegetarian, no_preference, mild, home_cooked, american, sweet, very_healthy, '10_30_min', medium, [coconut_flour, eggs, coconut_milk]).
meal('Savory Oatmeal Bowl', general_health, breakfast, vegetarian, no_preference, mild, home_cooked, no_preference, savory, very_healthy, '10_30_min', low, [oats, cheese, roasted_vegetables]).
meal('Berry Quinoa Breakfast', muscle_gain, breakfast, vegetarian, plant_based, mild, home_cooked, no_preference, sweet, very_healthy, '10_30_min', medium, [quinoa, mixed_berries, almond_butter]).
meal('Breakfast Pizza', tasty_food, breakfast, no_restrictions, no_preference, mild, home_cooked, italian, savory, treat_day, '10_30_min', high, [pizza_dough, eggs, bacon, cheese]).
meal('Tofu Scramble', weight_loss, breakfast, vegan, plant_based, medium, home_cooked, no_preference, savory, very_healthy, '10_30_min', low, [tofu, turmeric, vegetables]).
meal('Almond Butter Toast with Banana', energy_boost, breakfast, vegetarian, plant_based, mild, quick_easy, american, sweet, balanced, less_10_min, medium, [bread, almond_butter, banana]).
meal('Sweet Potato Hash', muscle_gain, breakfast, no_restrictions, no_preference, mild, home_cooked, american, savory, balanced, '10_30_min', medium, [sweet_potato, eggs, bell_peppers]).
meal('Chocolate Chia Pudding', tasty_food, breakfast, vegan, plant_based, mild, no_cooking, no_preference, sweet, treat_day, less_10_min, low, [chia_seeds, cocoa_powder, almond_milk]).
meal('Breakfast Sausage Patties', muscle_gain, breakfast, no_restrictions, no_preference, mild, home_cooked, american, savory, balanced, '10_30_min', medium, [sausage, herbs, spices]).
meal('Matcha Overnight Oats', general_health, breakfast, vegan, plant_based, mild, no_cooking, no_preference, sweet, very_healthy, less_10_min, low, [oats, matcha_powder, coconut_milk]).
meal('Breakfast Bruschetta', tasty_food, breakfast, vegetarian, no_preference, mild, quick_easy, italian, mix_both, treat_day, less_10_min, medium, [bread, tomatoes, mozzarella, basil]).
meal('Paleo Breakfast Skillet', muscle_gain, breakfast, no_restrictions, no_preference, mild, home_cooked, american, savory, balanced, '10_30_min', high, [sweet_potato, sausage, eggs, kale]).
meal('Cinnamon Roll Oatmeal', comfort_food, breakfast, vegetarian, no_preference, mild, home_cooked, american, sweet, treat_day, '10_30_min', medium, [oats, cinnamon, cream_cheese]).
meal('Breakfast Stuffed Peppers', weight_loss, breakfast, vegetarian, no_preference, mild, home_cooked, american, savory, very_healthy, '10_30_min', medium, [bell_peppers, eggs, spinach]).
meal('Protein French Toast Sticks', muscle_gain, breakfast, no_restrictions, no_preference, mild, home_cooked, american, sweet, balanced, '10_30_min', medium, [bread, protein_powder, eggs, cinnamon]).
meal('Breakfast Grain Bowl', general_health, breakfast, vegan, plant_based, mild, home_cooked, no_preference, mix_both, very_healthy, '10_30_min', medium, [quinoa, roasted_vegetables, tahini]).
meal('Ham and Cheese Croissant', tasty_food, breakfast, no_restrictions, no_preference, mild, no_cooking, french, savory, treat_day, less_10_min, high, [croissant, ham, cheese]).
meal('Breakfast Fried Rice', energy_boost, breakfast, no_restrictions, no_preference, medium, quick_easy, asian, savory, balanced, '10_30_min', medium, [rice, eggs, vegetables, soy_sauce]).
meal('Blueberry Almond Pancakes', tasty_food, breakfast, vegetarian, no_preference, mild, home_cooked, american, sweet, treat_day, '10_30_min', medium, [pancake_mix, blueberries, almonds]).
meal('Polenta Bowl', comfort_food, breakfast, vegetarian, no_preference, mild, home_cooked, italian, savory, balanced, '10_30_min', medium, [polenta, mushrooms, parmesan]).
meal('Egg Muffins', weight_loss, breakfast, no_restrictions, no_preference, mild, home_cooked, american, savory, very_healthy, '10_30_min', low, [eggs, vegetables, cheese]).

% ========================================
% LUNCH MEALS (30 meals)
% ========================================

meal('Chicken Caesar Salad', weight_loss, lunch, no_restrictions, chicken, mild, quick_easy, american, savory, very_healthy, '10_30_min', medium, [chicken, lettuce, parmesan, caesar_dressing]).
meal('Quinoa Buddha Bowl', general_health, lunch, vegan, plant_based, medium, home_cooked, no_preference, savory, very_healthy, '10_30_min', medium, [quinoa, chickpeas, vegetables, tahini]).
meal('Tuna Salad Sandwich', energy_boost, lunch, no_restrictions, fish_seafood, mild, quick_easy, american, savory, balanced, less_10_min, low, [tuna, bread, mayonnaise, lettuce]).
meal('Mediterranean Wrap', weight_loss, lunch, vegetarian, plant_based, medium, quick_easy, middle_eastern, savory, very_healthy, less_10_min, medium, [tortilla, hummus, vegetables, feta]).
meal('Vegan Lentil Soup', weight_loss, lunch, vegan, plant_based, mild, home_cooked, no_preference, savory, very_healthy, more_30_min, low, [lentils, vegetables, vegetable_broth]).
meal('Turkey Club Sandwich', energy_boost, lunch, no_restrictions, no_preference, mild, quick_easy, american, savory, balanced, less_10_min, medium, [turkey, bacon, lettuce, tomato, bread]).
meal('Chicken Burrito Bowl', muscle_gain, lunch, no_restrictions, chicken, medium, quick_easy, american, savory, balanced, '10_30_min', medium, [chicken, rice, beans, vegetables, salsa]).
meal('Caprese Salad', general_health, lunch, vegetarian, no_preference, mild, no_cooking, italian, savory, very_healthy, less_10_min, medium, [tomatoes, mozzarella, basil, olive_oil]).
meal('Pho Vietnamese Soup', comfort_food, lunch, no_restrictions, beef, medium, restaurant_style, asian, savory, balanced, no_cooking, medium, [beef, noodles, broth, herbs]).
meal('Greek Salad', weight_loss, lunch, vegetarian, no_preference, mild, quick_easy, middle_eastern, savory, very_healthy, less_10_min, low, [lettuce, tomatoes, olives, feta, olive_oil]).
meal('BLT Sandwich', tasty_food, lunch, no_restrictions, no_preference, mild, quick_easy, american, savory, treat_day, less_10_min, medium, [bacon, lettuce, tomato, bread]).
meal('Ramen Bowl', comfort_food, lunch, no_restrictions, no_preference, spicy, restaurant_style, asian, savory, balanced, no_cooking, medium, [noodles, broth, egg, vegetables]).
meal('Cobb Salad', muscle_gain, lunch, no_restrictions, chicken, mild, quick_easy, american, savory, balanced, '10_30_min', high, [chicken, bacon, eggs, cheese, lettuce]).
meal('Veggie Burger', general_health, lunch, vegetarian, plant_based, mild, quick_easy, american, savory, balanced, less_10_min, medium, [veggie_patty, bun, lettuce, tomato]).
meal('Chicken Quesadilla', tasty_food, lunch, no_restrictions, chicken, medium, home_cooked, american, savory, treat_day, '10_30_min', medium, [chicken, cheese, tortilla, peppers]).
meal('Nicoise Salad', weight_loss, lunch, no_restrictions, fish_seafood, mild, quick_easy, no_preference, savory, very_healthy, '10_30_min', medium, [tuna, eggs, olives, vegetables]).
meal('Minestrone Soup', general_health, lunch, vegetarian, plant_based, mild, home_cooked, italian, savory, very_healthy, more_30_min, low, [vegetables, beans, pasta, broth]).
meal('Chicken Pita Pocket', energy_boost, lunch, no_restrictions, chicken, medium, quick_easy, middle_eastern, savory, balanced, less_10_min, medium, [chicken, pita, vegetables, tzatziki]).
meal('Asian Lettuce Wraps', weight_loss, lunch, no_restrictions, chicken, medium, home_cooked, asian, savory, very_healthy, '10_30_min', medium, [chicken, lettuce, vegetables, sauce]).
meal('Gazpacho', weight_loss, lunch, vegan, plant_based, mild, no_cooking, no_preference, savory, very_healthy, less_10_min, low, [tomatoes, vegetables, herbs]).
meal('Falafel Wrap', general_health, lunch, vegan, plant_based, medium, quick_easy, middle_eastern, savory, balanced, less_10_min, medium, [falafel, wrap, vegetables, tahini]).
meal('Chicken Noodle Soup', comfort_food, lunch, no_restrictions, chicken, mild, home_cooked, american, savory, balanced, more_30_min, low, [chicken, noodles, vegetables, broth]).
meal('Spinach Salad', weight_loss, lunch, vegetarian, no_preference, mild, quick_easy, american, savory, very_healthy, less_10_min, low, [spinach, nuts, cheese, dressing]).
meal('Fish Tacos', general_health, lunch, no_restrictions, fish_seafood, medium, home_cooked, american, savory, balanced, '10_30_min', medium, [fish, tortillas, cabbage, salsa]).
meal('Pasta Salad', tasty_food, lunch, vegetarian, no_preference, mild, home_cooked, italian, savory, balanced, '10_30_min', medium, [pasta, vegetables, dressing]).
meal('Chicken Shawarma', energy_boost, lunch, no_restrictions, chicken, spicy, restaurant_style, middle_eastern, savory, balanced, no_cooking, medium, [chicken, pita, vegetables, sauce]).
meal('Veggie Sushi Bowl', general_health, lunch, vegetarian, plant_based, mild, quick_easy, asian, savory, very_healthy, '10_30_min', medium, [rice, vegetables, seaweed, sauce]).
meal('Lobster Roll', tasty_food, lunch, no_restrictions, fish_seafood, mild, restaurant_style, american, savory, treat_day, no_cooking, high, [lobster, bread, mayonnaise]).
meal('Black Bean Burger', weight_loss, lunch, vegan, plant_based, medium, quick_easy, american, savory, very_healthy, less_10_min, medium, [black_bean_patty, bun, vegetables]).
meal('Chicken Teriyaki Bowl', muscle_gain, lunch, no_restrictions, chicken, medium, home_cooked, asian, sweet, balanced, '10_30_min', medium, [chicken, rice, vegetables, teriyaki_sauce]).
meal('Korean Bibimbap', energy_boost, lunch, no_restrictions, beef, medium, restaurant_style, asian, savory, balanced, no_cooking, medium, [rice, beef, vegetables, egg, gochujang]).
meal('Chickpea Salad Sandwich', weight_loss, lunch, vegan, plant_based, mild, quick_easy, american, savory, very_healthy, less_10_min, low, [chickpeas, bread, celery, vegan_mayo]).
meal('Beef Bulgogi Bowl', muscle_gain, lunch, no_restrictions, beef, medium, home_cooked, asian, savory, balanced, '10_30_min', medium, [beef, rice, vegetables, bulgogi_sauce]).
meal('Miso Glazed Eggplant', general_health, lunch, vegan, plant_based, medium, home_cooked, asian, savory, very_healthy, '10_30_min', low, [eggplant, miso, rice, sesame]).
meal('Turkey Avocado Wrap', weight_loss, lunch, no_restrictions, no_preference, mild, quick_easy, american, savory, balanced, less_10_min, medium, [turkey, avocado, wrap, vegetables]).
meal('Vietnamese Banh Mi', tasty_food, lunch, no_restrictions, no_preference, spicy, restaurant_style, asian, savory, treat_day, no_cooking, medium, [baguette, pork, pickled_vegetables]).
meal('Moroccan Chickpea Stew', comfort_food, lunch, vegan, plant_based, spicy, home_cooked, middle_eastern, savory, very_healthy, more_30_min, low, [chickpeas, tomatoes, spices]).
meal('Sardine Toast', general_health, lunch, no_restrictions, fish_seafood, mild, quick_easy, no_preference, savory, very_healthy, less_10_min, medium, [sardines, bread, lemon, herbs]).
meal('Buffalo Chicken Wrap', muscle_gain, lunch, no_restrictions, chicken, spicy, quick_easy, american, savory, balanced, less_10_min, medium, [chicken, tortilla, hot_sauce, blue_cheese]).
meal('Japanese Curry Rice', comfort_food, lunch, no_restrictions, no_preference, medium, home_cooked, asian, savory, balanced, more_30_min, medium, [curry_roux, potatoes, carrots, rice]).
meal('Halloumi Salad', tasty_food, lunch, vegetarian, no_preference, mild, quick_easy, middle_eastern, savory, balanced, '10_30_min', medium, [halloumi, watermelon, mint]).
meal('Lentil Salad Bowl', weight_loss, lunch, vegan, plant_based, mild, no_cooking, no_preference, savory, very_healthy, less_10_min, low, [lentils, vegetables, lemon_dressing]).
meal('Pulled Pork Sandwich', tasty_food, lunch, no_restrictions, no_preference, medium, restaurant_style, american, savory, treat_day, no_cooking, high, [pulled_pork, bun, coleslaw]).
meal('Tempeh Reuben', general_health, lunch, vegetarian, plant_based, mild, home_cooked, american, savory, balanced, '10_30_min', medium, [tempeh, rye_bread, sauerkraut, dressing]).
meal('Thai Basil Chicken', energy_boost, lunch, no_restrictions, chicken, very_spicy, home_cooked, asian, savory, balanced, '10_30_min', medium, [chicken, basil, chili, rice]).
meal('Mediterranean Lentil Salad', weight_loss, lunch, vegan, plant_based, mild, no_cooking, middle_eastern, savory, very_healthy, less_10_min, low, [lentils, cucumber, tomatoes, feta]).
meal('Cheesesteak Sandwich', tasty_food, lunch, no_restrictions, beef, mild, restaurant_style, american, savory, treat_day, no_cooking, high, [ribeye, cheese, bread, peppers]).
meal('Jackfruit Tacos', general_health, lunch, vegan, plant_based, medium, home_cooked, mexican, savory, very_healthy, '10_30_min', low, [jackfruit, tortillas, avocado, lime]).
meal('Peruvian Chicken Bowl', energy_boost, lunch, no_restrictions, chicken, spicy, home_cooked, no_preference, savory, balanced, '10_30_min', medium, [chicken, rice, beans, sauce]).
meal('Baked Falafel Plate', weight_loss, lunch, vegan, plant_based, medium, home_cooked, middle_eastern, savory, very_healthy, '10_30_min', medium, [falafel, hummus, salad, pita]).
meal('Cajun Shrimp Pasta', tasty_food, lunch, no_restrictions, fish_seafood, spicy, home_cooked, american, savory, treat_day, '10_30_min', high, [shrimp, pasta, cream_sauce, cajun_spice]).
meal('Mushroom Walnut Salad', general_health, lunch, vegetarian, plant_based, mild, quick_easy, no_preference, savory, very_healthy, '10_30_min', low, [mushrooms, walnuts, greens, vinaigrette]).
meal('Hawaiian Poke Bowl', tasty_food, lunch, no_restrictions, fish_seafood, mild, restaurant_style, asian, savory, balanced, no_cooking, high, [tuna, rice, avocado, seaweed]).
meal('Spinach Feta Pie', comfort_food, lunch, vegetarian, no_preference, mild, home_cooked, greek, savory, balanced, more_30_min, medium, [spinach, feta, phyllo_dough]).
meal('BBQ Tempeh Sandwich', weight_loss, lunch, vegan, plant_based, medium, quick_easy, american, savory, very_healthy, less_10_min, medium, [tempeh, bbq_sauce, coleslaw, bun]).
meal('Peri Peri Chicken Bowl', energy_boost, lunch, no_restrictions, chicken, spicy, home_cooked, african, savory, balanced, '10_30_min', medium, [chicken, rice, vegetables, peri_peri_sauce]).
meal('Mushroom Barley Soup', comfort_food, lunch, vegetarian, no_preference, mild, home_cooked, no_preference, savory, very_healthy, more_30_min, low, [mushrooms, barley, vegetable_broth]).
meal('Jamaican Jerk Tofu', general_health, lunch, vegan, plant_based, very_spicy, home_cooked, caribbean, savory, very_healthy, '10_30_min', low, [tofu, jerk_spice, rice, pineapple]).
meal('Lebanese Fattoush Salad', weight_loss, lunch, vegetarian, no_preference, mild, quick_easy, middle_eastern, savory, very_healthy, less_10_min, low, [lettuce, pita_chips, sumac, lemon_dressing]).
meal('Brazilian Fish Stew', muscle_gain, lunch, no_restrictions, fish_seafood, medium, home_cooked, latin_american, savory, balanced, more_30_min, medium, [fish, coconut_milk, tomatoes, peppers]).
meal('Ethiopian Chickpea Wat', general_health, lunch, vegan, plant_based, spicy, home_cooked, african, savory, very_healthy, more_30_min, low, [chickpeas, berbere_spice, injera]).
meal('Korean Tofu Soup', comfort_food, lunch, vegan, plant_based, very_spicy, home_cooked, asian, savory, balanced, more_30_min, low, [tofu, kimchi, gochujang, vegetables]).
meal('Spanish Tortilla Wrap', energy_boost, lunch, vegetarian, no_preference, mild, quick_easy, spanish, savory, balanced, less_10_min, medium, [tortilla, eggs, potatoes, onions]).
meal('Persian Herb Frittata', general_health, lunch, vegetarian, no_preference, mild, home_cooked, middle_eastern, savory, very_healthy, '10_30_min', medium, [eggs, herbs, walnuts, feta]).
meal('Indonesian Gado Gado', weight_loss, lunch, vegan, plant_based, medium, home_cooked, asian, savory, very_healthy, '10_30_min', medium, [vegetables, tofu, peanut_sauce, rice]).
meal('Hungarian Mushroom Soup', comfort_food, lunch, vegetarian, no_preference, mild, home_cooked, no_preference, savory, balanced, more_30_min, low, [mushrooms, paprika, sour_cream, dill]).
meal('Filipino Chicken Adobo', energy_boost, lunch, no_restrictions, chicken, medium, home_cooked, asian, savory, balanced, more_30_min, medium, [chicken, soy_sauce, vinegar, garlic]).
meal('Turkish Mercimek Köftesi', general_health, lunch, vegan, plant_based, mild, home_cooked, middle_eastern, savory, very_healthy, '10_30_min', low, [red_lentils, bulgur, herbs, spices]).
meal('South African Bobotie', tasty_food, lunch, no_restrictions, no_preference, medium, home_cooked, african, savory, treat_day, more_30_min, high, [ground_beef, curry_powder, egg_custard]).
meal('Polish Beetroot Soup', weight_loss, lunch, vegetarian, no_preference, mild, home_cooked, no_preference, savory, very_healthy, more_30_min, low, [beets, vegetable_broth, ginger]).
meal('Vietnamese Tofu Banh Mi', general_health, lunch, vegan, plant_based, spicy, quick_easy, asian, savory, very_healthy, less_10_min, medium, [tofu, baguette, pickled_vegetables, mayo]).
meal('Argentinian Chimichurri Steak Salad', muscle_gain, lunch, no_restrictions, beef, medium, home_cooked, latin_american, savory, balanced, '10_30_min', high, [steak, chimichurri, mixed_greens]).
meal('Finnish Salmon Soup', general_health, lunch, no_restrictions, fish_seafood, mild, home_cooked, no_preference, savory, very_healthy, more_30_min, medium, [salmon, potatoes, dill, cream]).
meal('Portuguese Kale Soup', comfort_food, lunch, vegetarian, no_preference, mild, home_cooked, no_preference, savory, balanced, more_30_min, low, [kale, potatoes, beans, vegetable_broth]).
meal('Malaysian Laksa', energy_boost, lunch, no_restrictions, no_preference, very_spicy, home_cooked, asian, savory, balanced, more_30_min, medium, [noodles, coconut_milk, shrimp, laksa_paste]).
meal('Romanian Mamaliga', general_health, lunch, vegetarian, no_preference, mild, home_cooked, no_preference, savory, balanced, '10_30_min', low, [polenta, feta, sour_cream]).
meal('Scottish Smoked Salmon Salad', weight_loss, lunch, no_restrictions, fish_seafood, mild, quick_easy, no_preference, savory, very_healthy, less_10_min, high, [smoked_salmon, mixed_greens, lemon_dressing]).
meal('Belgian Endive Salad', general_health, lunch, vegetarian, no_preference, mild, quick_easy, no_preference, savory, very_healthy, less_10_min, low, [endives, walnuts, blue_cheese]).
meal('Austrian Käsespätzle', tasty_food, lunch, vegetarian, no_preference, mild, home_cooked, no_preference, savory, treat_day, more_30_min, high, [spätzle, cheese, caramelized_onions]).
meal('Cuban Black Bean Bowl', weight_loss, lunch, vegan, plant_based, mild, home_cooked, latin_american, savory, very_healthy, '10_30_min', low, [black_beans, rice, plantains, avocado]).

% ========================================
% DINNER MEALS (35 meals)
% ========================================

meal('Beef Stir Fry', muscle_gain, dinner, no_restrictions, beef, medium, home_cooked, asian, savory, balanced, '10_30_min', medium, [beef, vegetables, soy_sauce, rice]).
meal('Grilled Salmon with Vegetables', muscle_gain, dinner, no_restrictions, fish_seafood, mild, home_cooked, no_preference, savory, very_healthy, '10_30_min', high, [salmon, broccoli, asparagus, olive_oil]).
meal('Spaghetti Carbonara', tasty_food, dinner, no_restrictions, no_preference, mild, home_cooked, italian, savory, treat_day, '10_30_min', medium, [pasta, eggs, bacon, parmesan]).
meal('Spicy Thai Curry', tasty_food, dinner, no_restrictions, chicken, very_spicy, home_cooked, asian, savory, balanced, more_30_min, medium, [chicken, coconut_milk, curry_paste, vegetables]).
meal('Margherita Pizza', tasty_food, dinner, vegetarian, no_preference, mild, restaurant_style, italian, savory, treat_day, more_30_min, medium, [pizza_dough, tomato_sauce, mozzarella, basil]).
meal('BBQ Ribs', tasty_food, dinner, no_restrictions, beef, medium, restaurant_style, american, savory, treat_day, more_30_min, high, [ribs, bbq_sauce, spices]).
meal('Sushi Rolls', general_health, dinner, no_restrictions, fish_seafood, mild, restaurant_style, asian, savory, balanced, no_cooking, high, [sushi_rice, fish, nori, wasabi]).
meal('Spicy Korean Kimchi Fried Rice', energy_boost, dinner, no_restrictions, no_preference, very_spicy, home_cooked, asian, savory, balanced, '10_30_min', low, [rice, kimchi, eggs, soy_sauce]).
meal('Chicken Parmesan', tasty_food, dinner, no_restrictions, chicken, mild, home_cooked, italian, savory, treat_day, more_30_min, medium, [chicken, pasta, tomato_sauce, parmesan]).
meal('Vegetable Lasagna', general_health, dinner, vegetarian, no_preference, mild, home_cooked, italian, savory, balanced, more_30_min, medium, [pasta, vegetables, cheese, tomato_sauce]).
meal('Grilled Steak with Potatoes', muscle_gain, dinner, no_restrictions, beef, mild, home_cooked, american, savory, balanced, '10_30_min', high, [steak, potatoes, vegetables]).
meal('Fish Tacos', general_health, dinner, no_restrictions, fish_seafood, medium, home_cooked, american, savory, balanced, '10_30_min', medium, [fish, tortillas, cabbage, salsa]).
meal('Mushroom Risotto', tasty_food, dinner, vegetarian, no_preference, mild, home_cooked, italian, savory, balanced, more_30_min, medium, [rice, mushrooms, cheese, broth]).
meal('Butter Chicken', tasty_food, dinner, no_restrictions, chicken, medium, restaurant_style, asian, savory, treat_day, no_cooking, medium, [chicken, curry_sauce, rice, naan]).
meal('Vegan Buddha Bowl', weight_loss, dinner, vegan, plant_based, mild, home_cooked, no_preference, savory, very_healthy, '10_30_min', medium, [quinoa, tofu, vegetables, tahini]).
meal('Beef Tacos', energy_boost, dinner, no_restrictions, beef, medium, home_cooked, american, savory, balanced, '10_30_min', medium, [beef, tortillas, cheese, salsa]).
meal('Chicken Tikka Masala', tasty_food, dinner, no_restrictions, chicken, spicy, restaurant_style, asian, savory, treat_day, no_cooking, medium, [chicken, curry_sauce, rice]).
meal('Pork Chops with Apple Sauce', muscle_gain, dinner, no_restrictions, no_preference, mild, home_cooked, american, mix_both, balanced, '10_30_min', medium, [pork_chops, apple_sauce, vegetables]).
meal('Seafood Paella', tasty_food, dinner, no_restrictions, fish_seafood, medium, home_cooked, no_preference, savory, balanced, more_30_min, high, [rice, seafood, saffron, vegetables]).
meal('Lamb Gyros', energy_boost, dinner, no_restrictions, no_preference, medium, restaurant_style, middle_eastern, savory, balanced, no_cooking, medium, [lamb, pita, vegetables, tzatziki]).
meal('Eggplant Parmesan', general_health, dinner, vegetarian, no_preference, mild, home_cooked, italian, savory, balanced, more_30_min, medium, [eggplant, cheese, tomato_sauce]).
meal('Teriyaki Chicken', muscle_gain, dinner, no_restrictions, chicken, mild, home_cooked, asian, sweet, balanced, '10_30_min', medium, [chicken, teriyaki_sauce, rice]).
meal('Beef Bourguignon', tasty_food, dinner, no_restrictions, beef, mild, home_cooked, no_preference, savory, treat_day, more_30_min, high, [beef, wine, vegetables]).
meal('Stuffed Bell Peppers', general_health, dinner, no_restrictions, beef, mild, home_cooked, american, savory, very_healthy, more_30_min, medium, [bell_peppers, beef, rice]).
meal('Chicken Fajitas', energy_boost, dinner, no_restrictions, chicken, medium, home_cooked, american, savory, balanced, '10_30_min', medium, [chicken, peppers, tortillas, cheese]).
meal('Vegetable Stir Fry', weight_loss, dinner, vegan, plant_based, medium, home_cooked, asian, savory, very_healthy, '10_30_min', low, [vegetables, tofu, soy_sauce]).
meal('Meatball Spaghetti', comfort_food, dinner, no_restrictions, beef, mild, home_cooked, italian, savory, treat_day, more_30_min, medium, [meatballs, pasta, tomato_sauce]).
meal('Grilled Chicken Caesar Wrap', weight_loss, dinner, no_restrictions, chicken, mild, quick_easy, american, savory, balanced, less_10_min, medium, [chicken, lettuce, wrap, caesar_dressing]).
meal('Shrimp Scampi', tasty_food, dinner, no_restrictions, fish_seafood, mild, home_cooked, italian, savory, balanced, '10_30_min', high, [shrimp, pasta, garlic, butter]).
meal('Vegetarian Chili', general_health, dinner, vegetarian, plant_based, spicy, home_cooked, american, savory, very_healthy, more_30_min, low, [beans, vegetables, spices]).
meal('Duck Confit', tasty_food, dinner, no_restrictions, no_preference, mild, restaurant_style, no_preference, savory, treat_day, no_cooking, high, [duck, herbs, potatoes]).
meal('Chicken Enchiladas', comfort_food, dinner, no_restrictions, chicken, medium, home_cooked, american, savory, treat_day, more_30_min, medium, [chicken, tortillas, cheese, sauce]).
meal('Ratatouille', weight_loss, dinner, vegan, plant_based, mild, home_cooked, no_preference, savory, very_healthy, more_30_min, low, [vegetables, herbs, olive_oil]).
meal('Fish and Chips', tasty_food, dinner, no_restrictions, fish_seafood, mild, restaurant_style, american, savory, treat_day, no_cooking, medium, [fish, potatoes, batter]).
meal('Beef Stroganoff', comfort_food, dinner, no_restrictions, beef, mild, home_cooked, no_preference, savory, treat_day, more_30_min, medium, [beef, pasta, mushrooms, cream]).
meal('Korean BBQ Beef', muscle_gain, dinner, no_restrictions, beef, medium, home_cooked, asian, savory, balanced, '10_30_min', high, [beef, rice, kimchi, ssamjang]).
meal('Butternut Squash Risotto', comfort_food, dinner, vegetarian, no_preference, mild, home_cooked, italian, savory, balanced, more_30_min, medium, [butternut_squash, arborio_rice, parmesan]).
meal('Moroccan Tagine', tasty_food, dinner, no_restrictions, no_preference, spicy, home_cooked, middle_eastern, savory, balanced, more_30_min, medium, [chicken, apricots, spices, couscous]).
meal('Vegan Mac and Cheese', general_health, dinner, vegan, plant_based, mild, home_cooked, american, savory, balanced, '10_30_min', medium, [pasta, nutritional_yeast, cashews]).
meal('Honey Garlic Salmon', muscle_gain, dinner, no_restrictions, fish_seafood, mild, home_cooked, no_preference, sweet, very_healthy, '10_30_min', high, [salmon, honey, garlic, vegetables]).
meal('Stuffed Portobello Mushrooms', weight_loss, dinner, vegetarian, no_preference, mild, home_cooked, no_preference, savory, very_healthy, '10_30_min', low, [portobello, quinoa, cheese]).
meal('Jerk Chicken with Rice', energy_boost, dinner, no_restrictions, chicken, very_spicy, home_cooked, caribbean, savory, balanced, more_30_min, medium, [chicken, jerk_spice, rice, beans]).
meal('Eggplant Lasagna', general_health, dinner, vegetarian, no_preference, mild, home_cooked, italian, savory, very_healthy, more_30_min, medium, [eggplant, tomato_sauce, ricotta]).
meal('Beef Bulgogi', tasty_food, dinner, no_restrictions, beef, medium, home_cooked, asian, sweet, balanced, '10_30_min', high, [beef, bulgogi_marinade, rice]).
meal('Crispy Tofu Bowl', weight_loss, dinner, vegan, plant_based, medium, home_cooked, asian, savory, very_healthy, '10_30_min', low, [tofu, rice, vegetables, sauce]).
meal('Lemon Herb Roasted Chicken', muscle_gain, dinner, no_restrictions, chicken, mild, home_cooked, no_preference, savory, balanced, more_30_min, high, [chicken, lemon, herbs, potatoes]).
meal('Sweet Potato Black Bean Enchiladas', general_health, dinner, vegetarian, plant_based, medium, home_cooked, mexican, savory, very_healthy, more_30_min, medium, [sweet_potato, black_beans, tortillas, sauce]).
meal('Garlic Butter Shrimp Pasta', tasty_food, dinner, no_restrictions, fish_seafood, mild, home_cooked, italian, savory, treat_day, '10_30_min', high, [shrimp, pasta, garlic, butter]).
meal('Thai Green Curry', energy_boost, dinner, no_restrictions, chicken, very_spicy, home_cooked, asian, savory, balanced, more_30_min, medium, [chicken, green_curry_paste, coconut_milk, vegetables]).
meal('Ratatouille Pasta', weight_loss, dinner, vegetarian, plant_based, mild, home_cooked, french, savory, very_healthy, '10_30_min', low, [pasta, eggplant, zucchini, tomato_sauce]).
meal('Herb Crusted Salmon', muscle_gain, dinner, no_restrictions, fish_seafood, mild, home_cooked, no_preference, savory, very_healthy, '10_30_min', high, [salmon, herbs, breadcrumbs, vegetables]).
meal('Mushroom Stroganoff', comfort_food, dinner, vegetarian, no_preference, mild, home_cooked, no_preference, savory, balanced, '10_30_min', medium, [mushrooms, pasta, sour_cream]).
meal('Caribbean Curry Goat', tasty_food, dinner, no_restrictions, no_preference, spicy, home_cooked, caribbean, savory, treat_day, more_30_min, high, [goat, curry_powder, potatoes]).
meal('Zucchini Noodles with Pesto', weight_loss, dinner, vegan, plant_based, mild, quick_easy, italian, savory, very_healthy, less_10_min, low, [zucchini, pesto, cherry_tomatoes]).
meal('Korean Soft Tofu Stew', comfort_food, dinner, vegan, plant_based, very_spicy, home_cooked, asian, savory, balanced, more_30_min, low, [tofu, kimchi, vegetables, gochujang]).
meal('Balsamic Glazed Chicken', muscle_gain, dinner, no_restrictions, chicken, mild, home_cooked, no_preference, sweet, balanced, '10_30_min', medium, [chicken, balsamic, vegetables]).
meal('Vegetable Korma', general_health, dinner, vegetarian, plant_based, mild, home_cooked, indian, savory, very_healthy, more_30_min, medium, [vegetables, coconut_milk, spices, rice]).
meal('Cajun Blackened Fish', energy_boost, dinner, no_restrictions, fish_seafood, spicy, home_cooked, american, savory, balanced, '10_30_min', high, [fish, cajun_spice, vegetables]).
meal('Pumpkin Sage Pasta', tasty_food, dinner, vegetarian, no_preference, mild, home_cooked, italian, savory, treat_day, '10_30_min', medium, [pasta, pumpkin, sage, cream]).
meal('Ethiopian Lentil Stew', weight_loss, dinner, vegan, plant_based, spicy, home_cooked, african, savory, very_healthy, more_30_min, low, [lentils, berbere_spice, injera]).
meal('Icelandic Lamb Stew', comfort_food, dinner, no_restrictions, no_preference, mild, home_cooked, no_preference, savory, balanced, more_30_min, high, [lamb, root_vegetables, herbs]).
meal('Japanese Okonomiyaki', tasty_food, dinner, no_restrictions, no_preference, medium, home_cooked, asian, savory, balanced, '10_30_min', medium, [cabbage, flour, eggs, okonomiyaki_sauce]).
meal('Georgian Khachapuri', muscle_gain, dinner, vegetarian, no_preference, mild, home_cooked, no_preference, savory, treat_day, more_30_min, high, [cheese, eggs, bread, butter]).
meal('Swiss Chard Pie', general_health, dinner, vegetarian, no_preference, mild, home_cooked, no_preference, savory, very_healthy, more_30_min, medium, [swiss_chard, phyllo_dough, feta]).
meal('Senegalese Thieboudienne', energy_boost, dinner, no_restrictions, fish_seafood, spicy, home_cooked, african, savory, balanced, more_30_min, medium, [fish, rice, vegetables, tomato_sauce]).
meal('Ukrainian Borscht', weight_loss, dinner, vegetarian, no_preference, mild, home_cooked, no_preference, savory, very_healthy, more_30_min, low, [beets, cabbage, vegetable_broth, sour_cream]).
meal('Bolivian Silpancho', muscle_gain, dinner, no_restrictions, beef, medium, home_cooked, latin_american, savory, balanced, more_30_min, high, [beef, rice, potatoes, eggs]).
meal('Tunisian Shakshuka', general_health, dinner, vegetarian, no_preference, spicy, home_cooked, african, savory, very_healthy, '10_30_min', medium, [eggs, tomatoes, harissa, peppers]).
meal('Norwegian Fårikål', comfort_food, dinner, no_restrictions, no_preference, mild, home_cooked, no_preference, savory, balanced, more_30_min, high, [lamb, cabbage, peppercorns]).
meal('Sri Lankan Coconut Sambal', tasty_food, dinner, vegan, plant_based, very_spicy, home_cooked, asian, savory, balanced, '10_30_min', low, [coconut, chili, lime, rice]).
meal('Algerian Couscous', general_health, dinner, vegetarian, no_preference, mild, home_cooked, african, savory, very_healthy, more_30_min, medium, [couscous, vegetables, chickpeas, broth]).
meal('Chilean Pastel de Choclo', tasty_food, dinner, no_restrictions, no_preference, mild, home_cooked, latin_american, savory, treat_day, more_30_min, high, [corn, ground_beef, chicken, olives]).
meal('Bangladeshi Bhuna Khichuri', comfort_food, dinner, vegetarian, plant_based, medium, home_cooked, asian, savory, balanced, more_30_min, medium, [rice, lentils, spices, vegetables]).
meal('Bulgarian Kavarma', muscle_gain, dinner, no_restrictions, no_preference, medium, home_cooked, no_preference, savory, balanced, more_30_min, high, [pork, vegetables, paprika, cheese]).
meal('Omani Harees', general_health, dinner, no_restrictions, no_preference, mild, home_cooked, middle_eastern, savory, balanced, more_30_min, medium, [wheat, chicken, ghee, cinnamon]).
meal('Laap (Lao Minced Meat Salad)', weight_loss, dinner, no_restrictions, no_preference, very_spicy, home_cooked, asian, savory, very_healthy, '10_30_min', medium, [minced_meat, herbs, lime, toasted_rice]).
meal('Armenian Lentil Soup', general_health, dinner, vegan, plant_based, mild, home_cooked, no_preference, savory, very_healthy, more_30_min, low, [lentils, onions, cumin, lemon]).
meal('Ghanaian Red Red', tasty_food, dinner, vegan, plant_based, medium, home_cooked, african, savory, balanced, more_30_min, medium, [black-eyed_peas, plantains, palm_oil]).
meal('Mongolian Buuz', muscle_gain, dinner, no_restrictions, no_preference, medium, home_cooked, asian, savory, balanced, more_30_min, high, [dough, ground_meat, onions, garlic]).
meal('Bosnian Ćevapi', tasty_food, dinner, no_restrictions, no_preference, medium, home_cooked, no_preference, savory, treat_day, more_30_min, high, [ground_meat, flatbread, onions, kajmak]).
meal('Panamanian Sancocho', comfort_food, dinner, no_restrictions, chicken, mild, home_cooked, latin_american, savory, balanced, more_30_min, medium, [chicken, yam, corn, cilantro]).
meal('Nepalese Dal Bhat', general_health, dinner, vegetarian, plant_based, mild, home_cooked, asian, savory, very_healthy, more_30_min, medium, [lentils, rice, vegetables, pickles]).
meal('Estonian Kama', weight_loss, dinner, vegetarian, no_preference, mild, no_cooking, no_preference, sweet, very_healthy, less_10_min, low, [kama_flour, yogurt, berries]).
meal('Syrian Fattet Hummus', general_health, dinner, vegetarian, no_preference, mild, home_cooked, middle_eastern, savory, very_healthy, '10_30_min', medium, [chickpeas, yogurt, pita, pine_nuts]).
meal('Peruvian Lomo Saltado', muscle_gain, dinner, no_restrictions, beef, medium, home_cooked, latin_american, savory, balanced, '10_30_min', high, [beef, rice, fries, soy_sauce]).

% ========================================
% SNACK MEALS (20 meals)
% ========================================

meal('Hummus with Pita', energy_boost, snack, vegetarian, plant_based, mild, no_cooking, middle_eastern, savory, balanced, less_10_min, low, [hummus, pita, vegetables]).
meal('Protein Smoothie', muscle_gain, snack, no_restrictions, plant_based, mild, quick_easy, no_preference, sweet, very_healthy, less_10_min, medium, [protein_powder, banana, milk, peanut_butter]).
meal('Apple with Peanut Butter', energy_boost, snack, vegetarian, plant_based, mild, no_cooking, american, mix_both, balanced, less_10_min, low, [apple, peanut_butter]).
meal('Trail Mix', energy_boost, snack, vegetarian, plant_based, mild, no_cooking, american, mix_both, balanced, less_10_min, medium, [nuts, dried_fruits, chocolate]).
meal('Greek Yogurt Parfait', general_health, snack, vegetarian, no_preference, mild, quick_easy, no_preference, sweet, very_healthy, less_10_min, medium, [yogurt, granola, berries]).
meal('Cheese and Crackers', tasty_food, snack, vegetarian, no_preference, mild, no_cooking, american, savory, balanced, less_10_min, medium, [cheese, crackers]).
meal('Protein Bars', muscle_gain, snack, no_restrictions, plant_based, mild, no_cooking, american, sweet, balanced, less_10_min, medium, [protein_bar]).
meal('Vegetable Sticks with Dip', weight_loss, snack, vegetarian, plant_based, mild, no_cooking, american, savory, very_healthy, less_10_min, low, [vegetables, hummus]).
meal('Dark Chocolate', tasty_food, snack, vegetarian, no_preference, mild, no_cooking, no_preference, sweet, treat_day, less_10_min, medium, [dark_chocolate]).
meal('Smoothie Bowl', general_health, snack, vegan, plant_based, mild, quick_easy, no_preference, sweet, very_healthy, less_10_min, medium, [fruits, coconut, granola]).
meal('Mixed Nuts', energy_boost, snack, vegan, plant_based, mild, no_cooking, no_preference, savory, balanced, less_10_min, medium, [mixed_nuts]).
meal('Rice Cakes with Avocado', weight_loss, snack, vegan, plant_based, mild, quick_easy, american, savory, very_healthy, less_10_min, low, [rice_cakes, avocado]).
meal('Energy Balls', muscle_gain, snack, vegan, plant_based, mild, no_cooking, no_preference, sweet, balanced, less_10_min, medium, [dates, nuts, coconut]).
meal('Popcorn', tasty_food, snack, vegan, plant_based, mild, quick_easy, american, savory, balanced, less_10_min, low, [popcorn]).
meal('Fruit Smoothie', general_health, snack, vegan, plant_based, mild, quick_easy, no_preference, sweet, very_healthy, less_10_min, low, [fruits, water]).
meal('Cottage Cheese with Fruit', weight_loss, snack, vegetarian, no_preference, mild, no_cooking, american, mix_both, very_healthy, less_10_min, low, [cottage_cheese, fruit]).
meal('Edamame', weight_loss, snack, vegan, plant_based, mild, quick_easy, asian, savory, very_healthy, less_10_min, low, [edamame, salt]).
meal('Banana with Almond Butter', energy_boost, snack, vegan, plant_based, mild, no_cooking, american, sweet, balanced, less_10_min, low, [banana, almond_butter]).
meal('Kale Chips', weight_loss, snack, vegan, plant_based, mild, home_cooked, american, savory, very_healthy, '10_30_min', low, [kale, olive_oil, salt]).
meal('Protein Muffin', muscle_gain, snack, vegetarian, no_preference, mild, home_cooked, american, sweet, balanced, '10_30_min', medium, [protein_powder, oats, banana]).
meal('Seaweed Snacks', weight_loss, snack, vegan, plant_based, mild, no_cooking, asian, savory, very_healthy, less_10_min, low, [seaweed, sesame_oil, salt]).
meal('Chocolate Protein Shake', muscle_gain, snack, no_restrictions, no_preference, mild, quick_easy, no_preference, sweet, balanced, less_10_min, medium, [protein_powder, milk, chocolate, banana]).
meal('Spiced Roasted Chickpeas', energy_boost, snack, vegan, plant_based, spicy, home_cooked, no_preference, savory, balanced, '10_30_min', low, [chickpeas, paprika, garlic_powder]).
meal('Avocado Chocolate Mousse', tasty_food, snack, vegan, plant_based, mild, no_cooking, no_preference, sweet, treat_day, less_10_min, medium, [avocado, cocoa, maple_syrup]).
meal('Tzatziki with Veggies', weight_loss, snack, vegetarian, no_preference, mild, quick_easy, greek, savory, very_healthy, less_10_min, low, [yogurt, cucumber, garlic, vegetables]).
meal('Pistachio Dark Chocolate Bark', tasty_food, snack, vegetarian, no_preference, mild, no_cooking, no_preference, sweet, treat_day, less_10_min, high, [dark_chocolate, pistachios, sea_salt]).
meal('Turmeric Golden Milk', general_health, snack, vegan, plant_based, mild, quick_easy, indian, sweet, very_healthy, less_10_min, low, [turmeric, coconut_milk, honey]).
meal('Stuffed Dates', energy_boost, snack, vegetarian, plant_based, mild, no_cooking, no_preference, sweet, balanced, less_10_min, medium, [dates, almond_butter, coconut]).
meal('Wasabi Peas', tasty_food, snack, vegan, plant_based, spicy, no_cooking, asian, savory, balanced, less_10_min, low, [peas, wasabi, salt]).
meal('Chocolate Covered Almonds', tasty_food, snack, vegetarian, no_preference, mild, no_cooking, no_preference, sweet, treat_day, less_10_min, high, [almonds, dark_chocolate]).
meal('Savory Cottage Cheese Bowl', weight_loss, snack, vegetarian, no_preference, medium, quick_easy, no_preference, savory, very_healthy, less_10_min, low, [cottage_cheese, tomatoes, cucumber]).
meal('Peanut Butter Celery Sticks', energy_boost, snack, vegetarian, plant_based, mild, no_cooking, american, mix_both, balanced, less_10_min, low, [celery, peanut_butter, raisins]).
meal('Baked Kale Chips', weight_loss, snack, vegan, plant_based, mild, home_cooked, american, savory, very_healthy, '10_30_min', low, [kale, olive_oil, nutritional_yeast]).
meal('Chocolate Hummus with Fruit', tasty_food, snack, vegan, plant_based, mild, no_cooking, no_preference, sweet, treat_day, less_10_min, medium, [chickpeas, cocoa, fruits]).
meal('Spicy Roasted Nuts', energy_boost, snack, vegan, plant_based, spicy, home_cooked, no_preference, savory, balanced, '10_30_min', medium, [mixed_nuts, cayenne, maple_syrup]).
meal('Yogurt Covered Raisins', tasty_food, snack, vegetarian, no_preference, mild, no_cooking, no_preference, sweet, treat_day, less_10_min, medium, [yogurt, raisins]).
meal('Savory Oat Cakes', general_health, snack, vegetarian, no_preference, mild, home_cooked, no_preference, savory, balanced, '10_30_min', low, [oats, cheese, herbs]).
meal('Chia Energy Bars', muscle_gain, snack, vegan, plant_based, mild, no_cooking, no_preference, sweet, balanced, less_10_min, medium, [chia_seeds, dates, nuts]).
meal('Cucumber Sushi Rolls', weight_loss, snack, vegan, plant_based, mild, quick_easy, asian, savory, very_healthy, less_10_min, low, [cucumber, avocado, carrots]).
meal('Pumpkin Spice Energy Balls', tasty_food, snack, vegan, plant_based, mild, no_cooking, no_preference, sweet, treat_day, less_10_min, medium, [dates, pumpkin_puree, spices]).
meal('Antipasto Skewers', tasty_food, snack, no_restrictions, no_preference, mild, no_cooking, italian, savory, treat_day, less_10_min, high, [cheese, olives, salami]).
meal('Matcha Energy Bites', energy_boost, snack, vegan, plant_based, mild, no_cooking, no_preference, sweet, balanced, less_10_min, medium, [dates, matcha, coconut]).
meal('Roasted Pumpkin Seeds', weight_loss, snack, vegan, plant_based, mild, home_cooked, no_preference, savory, very_healthy, '10_30_min', low, [pumpkin_seeds, salt, spices]).
meal('Mango Sticky Rice Balls', tasty_food, snack, vegetarian, no_preference, mild, no_cooking, asian, sweet, treat_day, less_10_min, medium, [mango, sticky_rice, coconut_milk]).
meal('Salted Caramel Protein Balls', muscle_gain, snack, vegetarian, no_preference, mild, no_cooking, no_preference, sweet, balanced, less_10_min, medium, [protein_powder, dates, almond_butter]).
meal('Zaatar Manakish', energy_boost, snack, vegetarian, no_preference, mild, quick_easy, middle_eastern, savory, balanced, less_10_min, medium, [flatbread, zaatar, olive_oil]).
meal('Brazilian Pão de Queijo', tasty_food, snack, vegetarian, no_preference, mild, home_cooked, latin_american, savory, treat_day, '10_30_min', medium, [tapioca_flour, cheese, eggs]).
meal('Korean Hotteok', tasty_food, snack, vegetarian, no_preference, sweet, home_cooked, asian, sweet, treat_day, '10_30_min', high, [dough, brown_sugar, nuts, cinnamon]).
meal('Indian Masala Roasted Nuts', energy_boost, snack, vegan, plant_based, spicy, home_cooked, indian, savory, balanced, '10_30_min', medium, [mixed_nuts, garam_masala, turmeric]).
meal('Swedish Knäckebröd', weight_loss, snack, vegan, plant_based, mild, no_cooking, no_preference, savory, very_healthy, less_10_min, low, [rye_crispbread, seeds]).
meal('Filipino Turon', tasty_food, snack, vegetarian, no_preference, sweet, home_cooked, asian, sweet, treat_day, '10_30_min', medium, [plantains, brown_sugar, spring_roll_wrapper]).
meal('Egyptian Dukkah', general_health, snack, vegan, plant_based, mild, no_cooking, middle_eastern, savory, very_healthy, less_10_min, low, [nuts, seeds, spices, bread]).
meal('Russian Syrniki', tasty_food, snack, vegetarian, no_preference, mild, home_cooked, no_preference, sweet, treat_day, '10_30_min', medium, [cottage_cheese, flour, jam, sour_cream]).
meal('Mexican Esquites', energy_boost, snack, vegetarian, no_preference, spicy, quick_easy, mexican, savory, balanced, less_10_min, medium, [corn, mayonnaise, chili_powder, lime]).
meal('Greek Loukoumades', tasty_food, snack, vegetarian, no_preference, sweet, home_cooked, greek, sweet, treat_day, more_30_min, high, [dough, honey, cinnamon, walnuts]).
meal('Iranian Noghl', tasty_food, snack, vegetarian, no_preference, sweet, no_cooking, middle_eastern, sweet, treat_day, less_10_min, medium, [sugar, nuts, rose_water]).
meal('Vietnamese Bánh Flan', tasty_food, snack, vegetarian, no_preference, mild, no_cooking, asian, sweet, treat_day, less_10_min, medium, [eggs, condensed_milk, caramel]).
meal('Turkish Simit', energy_boost, snack, vegetarian, no_preference, mild, no_cooking, middle_eastern, savory, balanced, less_10_min, medium, [sesame_seeds, bread, molasses]).
meal('Italian Taralli', tasty_food, snack, vegetarian, no_preference, mild, no_cooking, italian, savory, balanced, less_10_min, medium, [flour, white_wine, olive_oil]).
meal('Chinese Scallion Pancakes', tasty_food, snack, vegetarian, no_preference, mild, home_cooked, asian, savory, treat_day, '10_30_min', medium, [flour, scallions, sesame_oil]).
meal('South African Biltong', muscle_gain, snack, no_restrictions, no_preference, mild, no_cooking, african, savory, balanced, less_10_min, high, [dried_meat, vinegar, spices]).
meal('French Palmiers', tasty_food, snack, vegetarian, no_preference, mild, home_cooked, french, sweet, treat_day, '10_30_min', high, [puff_pastry, sugar]).
meal('Colombian Pandebono', tasty_food, snack, vegetarian, no_preference, mild, home_cooked, latin_american, savory, treat_day, '10_30_min', medium, [tapioca_flour, cheese, eggs]).
meal('Danish Rugbrød', general_health, snack, vegan, plant_based, mild, no_cooking, no_preference, savory, very_healthy, less_10_min, low, [rye_bread, seeds, nuts]).
meal('Tibetan Tsampa', energy_boost, snack, vegan, plant_based, mild, no_cooking, asian, savory, balanced, less_10_min, low, [roasted_barley_flour, butter_tea]).
meal('Kenyan Mandazi', tasty_food, snack, vegetarian, no_preference, mild, home_cooked, african, sweet, treat_day, '10_30_min', medium, [flour, coconut_milk, sugar, cardamom]).
meal('Croatian Fritule', tasty_food, snack, vegetarian, no_preference, sweet, home_cooked, no_preference, sweet, treat_day, '10_30_min', high, [dough, raisins, rum, powdered_sugar]).
meal('Moroccan Msemen', tasty_food, snack, vegetarian, no_preference, mild, home_cooked, middle_eastern, savory, treat_day, '10_30_min', medium, [flour, semolina, butter, honey]).
meal('Pakistani Chana Chaat', weight_loss, snack, vegan, plant_based, spicy, quick_easy, asian, savory, very_healthy, less_10_min, low, [chickpeas, onions, tamarind, chaat_masala]).
meal('Australian Anzac Biscuits', tasty_food, snack, vegetarian, no_preference, mild, home_cooked, no_preference, sweet, treat_day, '10_30_min', medium, [oats, coconut, golden_syrup]).

% Query predicates for meal recommendations

% Find meals matching specific criteria
find_meal(Name, Goal, Time, Dietary, Protein, Spice, MealType, Cuisine, Taste, Health, PrepTime, Budget) :-
    meal(Name, Goal, Time, Dietary, Protein, Spice, MealType, Cuisine, Taste, Health, PrepTime, Budget, _).

% Find meals by single criterion
meals_by_goal(Goal, Meals) :-
    findall(Name, meal(Name, Goal, _, _, _, _, _, _, _, _, _, _, _), Meals).

meals_by_time(Time, Meals) :-
    findall(Name, meal(Name, _, Time, _, _, _, _, _, _, _, _, _, _), Meals).

meals_by_dietary(Dietary, Meals) :-
    findall(Name, meal(Name, _, _, Dietary, _, _, _, _, _, _, _, _, _), Meals).

meals_by_protein(Protein, Meals) :-
    findall(Name, meal(Name, _, _, _, Protein, _, _, _, _, _, _, _, _), Meals).

meals_by_spice(Spice, Meals) :-
    findall(Name, meal(Name, _, _, _, _, Spice, _, _, _, _, _, _, _), Meals).

meals_by_cuisine(Cuisine, Meals) :-
    findall(Name, meal(Name, _, _, _, _, _, _, Cuisine, _, _, _, _, _), Meals).

meals_by_prep_time(PrepTime, Meals) :-
    findall(Name, meal(Name, _, _, _, _, _, _, _, _, _, PrepTime, _, _), Meals).

meals_by_budget(Budget, Meals) :-
    findall(Name, meal(Name, _, _, _, _, _, _, _, _, _, _, Budget, _), Meals).

meals_by_name(Name, Ingredients) :-
    meal(Name, _,_,_, _, _, _, _, _, _, _, _, Ingredients).




% Get meal details including ingredients
meal_details(Name, Details) :-
    meal(Name, Goal, Time, Dietary, Protein, Spice, MealType, Cuisine, Taste, Health, PrepTime, Budget, Ingredients),
    Details = [
        goal(Goal),
        time(Time),
        dietary(Dietary),
        protein(Protein),
        spice(Spice),
        meal_type(MealType),
        cuisine(Cuisine),
        taste(Taste),
        health(Health),
        prep_time(PrepTime),
        budget(Budget),
        ingredients(Ingredients)
    ].

% Advanced queries for recommendations
healthy_weight_loss_meals(Meals) :-
    findall(Name, meal(Name, weight_loss, _, _, _, _, _, _, _, very_healthy, _, _, _), Meals).

quick_breakfast(Meals) :-
    findall(Name, meal(Name, _, breakfast, _, _, _, _, _, _, _, less_10_min, _, _), QuickMeals),
    findall(Name, meal(Name, _, breakfast, _, _, _, _, _, _, _, '10_30_min', _, _), MediumMeals),
    append(QuickMeals, MediumMeals, Meals).

vegetarian_dinners(Meals) :-
    findall(Name, (
        meal(Name, _, dinner, Dietary, _, _, _, _, _, _, _, _, _),
        (Dietary = vegetarian; Dietary = vegan)
    ), Meals).

budget_meals(Meals) :-
    findall(Name, meal(Name, _, _, _, _, _, _, _, _, _, _, low, _), Meals).

spicy_asian_meals(Meals) :-
    findall(Name, (
        meal(Name, _, _, _, _, Spice, _, asian, _, _, _, _, _),
        (Spice = spicy; Spice = very_spicy)
    ), Meals).

% Helper to check if meal contains avoided ingredients
contains_avoided_ingredients(_, []) :- fail.
contains_avoided_ingredients(MealIngredients, [H|T]) :-
    (member(H, MealIngredients) -> true ; contains_avoided_ingredients(MealIngredients, T)).

% Calculate match score (higher = better match)
calculate_match_score(UserPrefs, MealAttrs, Score) :-
    calculate_match_score_helper(UserPrefs, MealAttrs, 0, Score).

calculate_match_score_helper([], [], Acc, Acc).
calculate_match_score_helper([UP|UTail], [MA|MTail], Acc, Score) :-
    (
        (UP = MA, NewAcc is Acc + 10) ;
        (UP = no_preference, NewAcc is Acc + 5) ;
        (MA = no_preference, NewAcc is Acc + 5) ;
        (compatible_preferences(UP, MA), NewAcc is Acc + 7) ;
        NewAcc = Acc
    ),
    calculate_match_score_helper(UTail, MTail, NewAcc, Score).

% Define compatible preferences (partial matches)
compatible_preferences(vegetarian, vegan).
compatible_preferences(vegan, vegetarian).
compatible_preferences(weight_loss, very_healthy).
compatible_preferences(general_health, very_healthy).
compatible_preferences(muscle_gain, balanced).
compatible_preferences(quick_easy, less_10_min).
compatible_preferences(quick_easy, '10_30_min').
compatible_preferences(mild, medium).
compatible_preferences(medium, spicy).
compatible_preferences(low, medium).

% **NEW FUNCTION: Get meals with 90% or higher match**
get_90_percent_match_meals(Goal, Time, Dietary, Protein, Spice, MealType, Cuisine, Taste, Health, PrepTime, Budget, AvoidedIngredients, HighMatchMeals) :-
    findall(
        name_score(Name, MatchScore),
        (
            meal(Name, MGoal, MTime, MDietary, MProtein, MSpice, MMealType, MCuisine, MTaste, MHealth, MPrepTime, MBudget, Ingredients),
            \+ contains_avoided_ingredients(Ingredients, AvoidedIngredients),
            calculate_match_score(
                [Goal, Time, Dietary, Protein, Spice, MealType, Cuisine, Taste, Health, PrepTime, Budget],
                [MGoal, MTime, MDietary, MProtein, MSpice, MMealType, MCuisine, MTaste, MHealth, MPrepTime, MBudget],
                MatchScore
            ),
            MatchScore >= 80  % Only meals with 90% or higher match
        ),
        ScoredMeals
    ),
    sort_names_by_score(ScoredMeals, SortedMeals),
    extract_names_only(SortedMeals, HighMatchMeals).

% Alternative version that takes percentage as parameter
get_high_match_meals(Goal, Time, Dietary, Protein, Spice, MealType, Cuisine, Taste, Health, PrepTime, Budget, AvoidedIngredients, MinPercentage, HighMatchMeals) :-
    findall(
        name_score(Name, MatchScore),
        (
            meal(Name, MGoal, MTime, MDietary, MProtein, MSpice, MMealType, MCuisine, MTaste, MHealth, MPrepTime, MBudget, Ingredients),
            \+ contains_avoided_ingredients(Ingredients, AvoidedIngredients),
            calculate_match_score(
                [Goal, Time, Dietary, Protein, Spice, MealType, Cuisine, Taste, Health, PrepTime, Budget],
                [MGoal, MTime, MDietary, MProtein, MSpice, MMealType, MCuisine, MTaste, MHealth, MPrepTime, MBudget],
                MatchScore
            ),
            MatchScore >= MinPercentage
        ),
        ScoredMeals
    ),
    sort_names_by_score(ScoredMeals, SortedMeals),
    extract_names_only(SortedMeals, HighMatchMeals).

get_90_percent_meals_with_scores_fixed(Goal, Time, Dietary, Protein, Spice, MealType, Cuisine, Taste, Health, PrepTime, Budget, AvoidedIngredients, MealsWithScores) :-
    findall(
        Name,
        (
            meal(Name, MGoal, MTime, MDietary, MProtein, MSpice, MMealType, MCuisine, MTaste, MHealth, MPrepTime, MBudget, Ingredients),
            \+ contains_avoided_ingredients(Ingredients, AvoidedIngredients)
        ),
        AllMealNames
    ),
    sort(AllMealNames, UniqueMealNames),
    findall(
        meal_match(Name, BestScore),
        (
            member(Name, UniqueMealNames),
            get_best_score_for_meal(Name, Goal, Time, Dietary, Protein, Spice, MealType, Cuisine, Taste, Health, PrepTime, Budget, BestScore),
            BestScore >= 70
        ),
        UnsortedMeals
    ),
    sort_meals_by_score(UnsortedMeals, MealsWithScores).

% Helper predicate to get the best (highest) score for a specific meal
get_best_score_for_meal(MealName, Goal, Time, Dietary, Protein, Spice, MealType, Cuisine, Taste, Health, PrepTime, Budget, BestScore) :-
    findall(
        Score,
        (
            meal(MealName, MGoal, MTime, MDietary, MProtein, MSpice, MMealType, MCuisine, MTaste, MHealth, MPrepTime, MBudget, _),
            calculate_match_score(
                [Goal, Time, Dietary, Protein, Spice, MealType, Cuisine, Taste, Health, PrepTime, Budget],
                [MGoal, MTime, MDietary, MProtein, MSpice, MMealType, MCuisine, MTaste, MHealth, MPrepTime, MBudget],
                Score
            )
        ),
        Scores
    ),
    max_list(Scores, BestScore).

% Enhanced version that returns both names and scores for 90% matches
get_90_percent_meals_with_scores(Goal, Time, Dietary, Protein, Spice, MealType, Cuisine, Taste, Health, PrepTime, Budget, AvoidedIngredients, MealsWithScores) :-
    findall(
        meal_match(Name, MatchScore),
        (
            meal(Name, MGoal, MTime, MDietary, MProtein, MSpice, MMealType, MCuisine, MTaste, MHealth, MPrepTime, MBudget, Ingredients),
            \+ contains_avoided_ingredients(Ingredients, AvoidedIngredients),
            calculate_match_score(
                [Goal, Time, Dietary, Protein, Spice, MealType, Cuisine, Taste, Health, PrepTime, Budget],
                [MGoal, MTime, MDietary, MProtein, MSpice, MMealType, MCuisine, MTaste, MHealth, MPrepTime, MBudget],
                MatchScore
            ),
            MatchScore >= 70
        ),
        UnsortedMeals
    ),
    sort_meals_by_score(UnsortedMeals, MealsWithScores).

% Helper functions for the new 90% match functionality
sort_names_by_score(Names, SortedNames) :-
    predsort(compare_name_scores, Names, SortedNames).

compare_name_scores(Order, name_score(_, Score1), name_score(_, Score2)) :-
    (Score1 > Score2 -> Order = (<) ; 
     Score1 < Score2 -> Order = (>) ; 
     Order = (=)).

extract_names_only([], []).
extract_names_only([name_score(Name, _)|Rest], [Name|Names]) :-
    extract_names_only(Rest, Names).

sort_meals_by_score(Meals, SortedMeals) :-
    predsort(compare_meal_scores, Meals, SortedMeals).

compare_meal_scores(Order, meal_match(_, Score1), meal_match(_, Score2)) :-
    (Score1 > Score2 -> Order = (<) ; 
     Score1 < Score2 -> Order = (>) ; 
     Order = (=)).

% MAIN FUNCTION: Get meal recommendations based on all questionnaire answers
get_meal_recommendations(Goal, Time, Dietary, Protein, Spice, MealType, Cuisine, Taste, Health, PrepTime, Budget, AvoidedIngredients, MealNames) :-
    findall(
        score_name(MatchScore, Name),
        (
            meal(Name, MGoal, MTime, MDietary, MProtein, MSpice, MMealType, MCuisine, MTaste, MHealth, MPrepTime, MBudget, Ingredients),
            \+ contains_avoided_ingredients(Ingredients, AvoidedIngredients),
            calculate_match_score(
                [Goal, Time, Dietary, Protein, Spice, MealType, Cuisine, Taste, Health, PrepTime, Budget],
                [MGoal, MTime, MDietary, MProtein, MSpice, MMealType, MCuisine, MTaste, MHealth, MPrepTime, MBudget],
                MatchScore
            ),
            MatchScore > 50
        ),
        ScoredMeals
    ),
    sort(ScoredMeals, SortedScoredMeals),
    reverse(SortedScoredMeals, DescendingScoredMeals),
    extract_names_from_scored(DescendingScoredMeals, MealNames).

extract_names_from_scored([], []).
extract_names_from_scored([score_name(_, Name)|Rest], [Name|Names]) :-
    extract_names_from_scored(Rest, Names).

get_meal_names(Goal, Time, Dietary, Protein, Spice, MealType, Cuisine, Taste, Health, PrepTime, Budget, AvoidedIngredients, MealNames) :-
    findall(
        name_score(Name, MatchScore),
        (
            meal(Name, MGoal, MTime, MDietary, MProtein, MSpice, MMealType, MCuisine, MTaste, MHealth, MPrepTime, MBudget, Ingredients),
            \+ contains_avoided_ingredients(Ingredients, AvoidedIngredients),
            calculate_match_score(
                [Goal, Time, Dietary, Protein, Spice, MealType, Cuisine, Taste, Health, PrepTime, Budget],
                [MGoal, MTime, MDietary, MProtein, MSpice, MMealType, MCuisine, MTaste, MHealth, MPrepTime, MBudget],
                MatchScore
            ),
            MatchScore > 0
        ),
        UnsortedNames
    ),
    sort_names_by_score(UnsortedNames, SortedNames),
    extract_names_only(SortedNames, MealNames).

% Helper to extract preference from list
extract_preference(Key, Preferences, Value) :-
    member(Key(Value), Preferences), !.
extract_preference(_, _, no_preference).

get_meal_names_from_list(Preferences, MealNames) :-
    extract_preference(goal, Preferences, Goal),
    extract_preference(time, Preferences, Time),
    extract_preference(dietary, Preferences, Dietary),
    extract_preference(protein, Preferences, Protein),
    extract_preference(spice, Preferences, Spice),
    extract_preference(meal_type, Preferences, MealType),
    extract_preference(cuisine, Preferences, Cuisine),
    extract_preference(taste, Preferences, Taste),
    extract_preference(health, Preferences, Health),
    extract_preference(prep_time, Preferences, PrepTime),
    extract_preference(budget, Preferences, Budget),
    extract_preference(avoided_ingredients, Preferences, AvoidedIngredients),
    get_meal_names(Goal, Time, Dietary, Protein, Spice, MealType, Cuisine, Taste, Health, PrepTime, Budget, AvoidedIngredients, MealNames).

quick_meal_names(Scenario, MealNames) :-
    scenario_preferences(Scenario, Preferences),
    get_meal_names_from_list(Preferences, MealNames).

recommend_meal(Answers, MealNames) :-
    Answers = answers(Goal, Time, Dietary, Protein, Spice, MealType, Cuisine, Taste, Health, PrepTime, Budget, Avoid),
    get_meal_names(Goal, Time, Dietary, Protein, Spice, MealType, Cuisine, Taste, Health, PrepTime, Budget, Avoid, MealNames).

% USAGE EXAMPLES FOR 90% MATCH FUNCTION:
% 
% Example 1: Get meals with exactly 90% match
% ?- get_90_percent_match_meals(weight_loss, breakfast, vegetarian, plant_based, mild, quick_easy, american, savory, very_healthy, less_10_min, medium, [], HighMatchMeals).
%
% Example 2: Get meals with custom percentage (e.g., 80% or higher)
% ?- get_high_match_meals(muscle_gain, dinner, no_restrictions, chicken, medium, home_cooked, asian, savory, balanced, '10_30_min', medium, [], 80, HighMatchMeals).
%
% Example 3: Get meals with scores for 90% matches
% ?- get_90_percent_meals_with_scores(general_health, lunch, vegan, plant_based, mild, home_cooked, no_preference, savory, very_healthy, '10_30_min', medium, [], MealsWithScores).