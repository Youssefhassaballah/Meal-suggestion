import subprocess
from pyswip import Prolog

class MealPlanner:
    def __init__(self, answers, prolog_file="data.pl"):
        self.prolog_file = prolog_file
        self.prolog = Prolog()
        self.prolog.consult(self.prolog_file)
        self.answers = answers
        
    def get_ingredients(self, meal_name):
        """
        Get ingredients for a specific meal.
        Returns a list of ingredients or an empty list if the meal is not found.
        """
        
        query = f"meals_by_name({meal_name}, Ingredients)"
        result = list(self.prolog.query(query))
        print(result)
        if result:
            return result[0]['Ingredients']
        else:
            return []

    def run_prolog_query(self, query):
        return list(self.prolog.query(query))
    
    def generate_recommend_meal_query(self):
        """
        Generate a Prolog query for the get_meal_recommendations function.
        Returns a query string that can be executed in Prolog.
        """

        # Mapping from Python answer keys to Prolog parameter order
        # Order matches: Goal, Time, Dietary, Protein, Spice, MealType, Cuisine, Taste, Health, PrepTime, Budget, AvoidedIngredients
        answer_mapping = [
            'goal',
            'time', 
            'dietary',
            'protein',
            'spice',
            'meal_type',
            'cuisine',
            'taste',
            'health',
            'prep_time',
            'budget'
        ]

        # Build parameter list for the Prolog query
        parameters = []

        for key in answer_mapping:
            value = self.answers.get(key, 'no_preference')

            # Handle special formatting cases
            if key == 'prep_time' and '_' in str(value):
                # Quote time ranges like '10_30_min'
                formatted_value = f"'{value}'"
            elif value is None or value == '' or value == 'none':
                formatted_value = 'no_preference'
            else:
                formatted_value = str(value)

            parameters.append(formatted_value)

        # Handle avoided ingredients (must be a proper Prolog list)
        avoided_ingredients = self.answers.get('avoided_ingredients', [])
        if isinstance(avoided_ingredients, list):
            if avoided_ingredients:
                # Convert to Prolog list format: [item1, item2, item3]
                avoided_list = '[' + ', '.join(str(item) for item in avoided_ingredients) + ']'
            else:
                avoided_list = '[]'  # Empty list
        else:
            avoided_list = '[]'

        parameters.append(avoided_list)

        # Add the result variable
        parameters.append('RecommendedMeals')

        # Generate the complete query
        query = f"get_90_percent_meals_with_scores_fixed({', '.join(parameters)})"

        return query

    def suggest_meal(self):
        query = self.generate_recommend_meal_query()
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