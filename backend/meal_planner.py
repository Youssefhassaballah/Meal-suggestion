import subprocess

class MealPlanner:
    def __init__(self, prolog_file="data.pl"):
        self.prolog_file = prolog_file
        
    def run_prolog_query(self, query):
        """Run a Prolog query and return the results"""
        cmd = ['swipl', '-q', '-f', self.prolog_file, '-g', query, '-t', 'halt']
        try:
            result = subprocess.run(cmd, capture_output=True, text=True)
            if result.returncode != 0:
                print("Error running Prolog query:", result.stderr)
                return []
            return [line.strip() for line in result.stdout.splitlines() if line.strip()]
        except FileNotFoundError:
            print("SWI-Prolog not found. Please install it from https://www.swi-prolog.org")
            return []
    
    def suggest_meal(self, ingredients, meal_type=None):
        """Suggest meals based on available ingredients"""
        ingredients_str = f"[{','.join(ingredients)}]"
        if meal_type:
            query = f"findall(Meal,suggest_meal_type({ingredients_str},{meal_type},Meal),L),member(M,L),write(M),nl,fail"
        else:
            query = f"findall(Meal,(suggest_meal({ingredients_str},Meal,_)),L),member(M,L),write(M),nl,fail"
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