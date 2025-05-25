import subprocess
from pyswip import Prolog

class MealPlanner:
    def __init__(self, answers, prolog_file="data2.pl"):
        self.prolog_file = prolog_file
        self.prolog = Prolog()
        self.prolog.consult(self.prolog_file)
        self.answers = answers

    def run_prolog_query(self, query):
        return list(self.prolog.query(query))
    
    def generate_prolog_query(self):
    # Map Python attribute names to Prolog predicate names
        attribute_mapping = {
        'goal': None,  # Handled specially in the goal processing
        'time': 'time',
        'dietary': 'dietary',
        'protein': 'protein',
        'spice': 'spice',
        'meal_type': 'meal_type',
        'cuisine': 'cuisine',
        'taste': 'taste',
        'health': 'health',
        'prep_time': 'prep_time',
        'avoided_ingredients': 'avoided_ingredients',
        'budget': 'budget'
        }

        # Process special cases first (like 'goal')
        prolog_options = []

        # Handle muscle_gain goal (prioritize high protein)
        if self.answers.get('goal') == 'muscle_gain':
            if self.answers.get('protein') is None:
                prolog_options.append("protein(plant_based)")

        # Add all other attributes
        for py_attr, pl_attr in attribute_mapping.items():
            if pl_attr and py_attr in self.answers:
                value = self.answers[py_attr]
                # Handle special cases for values
                if py_attr == 'avoided_ingredients':
                    # Split comma-separated ingredients if needed
                    ingredients = [ing.strip() for ing in value.split(',')]
                    for ing in ingredients:
                        prolog_options.append(f"avoided_ingredients({ing})")
                else:
                    prolog_options.append(f"{pl_attr}({value})")

        # Build the Prolog query
        options_str = ", ".join(prolog_options)
        query = f"suggest_meal([{options_str}], Meal)"

        return query


    def suggest_meal(self):
        query = self.generate_prolog_query()
        print(f"Generated Prolog query: {query}")
        return self.run_prolog_query(query)

    def vegetarian_meals(self):
        """Get all vegetarian meals"""
        return self.run_prolog_query("findall(Meal,vegetarian_meal(Meal),L),member(M,L),write(M),nl,fail")
    
    def high_protein_meals(self):
        """Get all high protein meals"""
        return self.run_prolog_query("findall(Meal,high_protein_meal(Meal),L),member(M,L),write(M),nl,fail")
    
    def quick_meals(self):
        """Get all quick meals (3 ingredients or less)"""
        return self.run_prolog_query("findall(Meal,quick_meal(Meal),L),member(M,L),write(M),nl,fail")
    
    def avoid_ingredients(self, ingredients_to_avoid):
        """Get meals that avoid certain ingredients"""
        ingredients_str = f"[{','.join(ingredients_to_avoid)}]"
        return self.run_prolog_query(f"findall(Meal,avoid_ingredients(Meal,{ingredients_str}),L),member(M,L),write(M),nl,fail")

def main():

    
    planner = MealPlanner()
   
    print("=== Meal Planning System ===")
    print("\n1. Suggest meals by ingredients")
    print("2. Vegetarian meals")
    print("3. High protein meals")
    print("4. Quick meals")
    print("5. Avoid certain ingredients")
    print("0. Exit")
    
    while True:
        choice = input("\nEnter your choice (0-5): ").strip()
        
        if choice == "0":
            print("Goodbye!")
            break
            
        elif choice == "1":
            ingredients = input("Enter ingredients (comma separated, e.g. 'bread,eggs,milk'): ").strip()
            ingredients_list = [i.strip() for i in ingredients.split(",")]
            meal_type = input("Optional - enter meal type (breakfast/lunch/dinner/snack/dessert or leave blank): ").strip().lower()
            
            if meal_type and meal_type in ['breakfast', 'lunch', 'dinner', 'snack', 'dessert']:
                results = planner.suggest_meal(ingredients_list, meal_type)
            elif meal_type:
                print("Invalid meal type. Showing all matching meals.")
                results = planner.suggest_meal(ingredients_list)
            else:
                results = planner.suggest_meal(ingredients_list)
                
            print(f"\nSuggested meals with {ingredients}:")
            if results:
                for i, meal in enumerate(results, 1):
                    print(f"{i}. {meal}")
            else:
                print("No meals found matching your ingredients.")
            
        elif choice == "2":
            print("\nVegetarian meals:")
            results = planner.vegetarian_meals()
            if results:
                for i, meal in enumerate(results, 1):
                    print(f"{i}. {meal}")
            else:
                print("No vegetarian meals found.")
            
        elif choice == "3":
            print("\nHigh protein meals:")
            results = planner.high_protein_meals()
            if results:
                for i, meal in enumerate(results, 1):
                    print(f"{i}. {meal}")
            else:
                print("No high protein meals found.")
            
        elif choice == "4":
            print("\nQuick meals (3 ingredients or less):")
            results = planner.quick_meals()
            if results:
                for i, meal in enumerate(results, 1):
                    print(f"{i}. {meal}")
            else:
                print("No quick meals found.")
            
        elif choice == "5":
            ingredients = input("Enter ingredients to avoid (comma separated): ").strip()
            ingredients_list = [i.strip() for i in ingredients.split(",")]
            print(f"\nMeals without {ingredients}:")
            results = planner.avoid_ingredients(ingredients_list)
            if results:
                for i, meal in enumerate(results, 1):
                    print(f"{i}. {meal}")
            else:
                print(f"No meals found that avoid {ingredients}")
            
        else:
            print("Invalid choice. Please try again.")

if __name__ == "__main__":
    main()