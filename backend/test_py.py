#!/usr/bin/env python3
"""
PySwip Test Script for Meal Recommendation System
Run this script to test all the meal recommendation queries
"""

from pyswip import Prolog
import json
from typing import List, Dict, Any

class MealRecommendationTester:
    def __init__(self, prolog_file_path: str):
        """Initialize the Prolog engine and load the knowledge base"""
        self.prolog = Prolog()
        self.prolog_file = prolog_file_path
        
        # Load the Prolog file
        try:
            self.prolog.consult(prolog_file_path)
            print(f"✅ Successfully loaded Prolog file: {prolog_file_path}")
        except Exception as e:
            print(f"❌ Error loading Prolog file: {e}")
            
    def test_basic_meal_queries(self):
        """Test basic meal database queries"""
        print("\n" + "="*50)
        print("TESTING BASIC MEAL QUERIES")
        print("="*50)
        
        # Test 1: Find all breakfast meals
        print("\n1. All breakfast meals:")
        try:
            results = list(self.prolog.query("meal(Name, breakfast, _, _, _, _, _, _, _, _, _, _, _, _)"))
            breakfast_meals = [result['Name'] for result in results]
            print(f"Found {len(breakfast_meals)} breakfast meals:")
            for meal in breakfast_meals[:10]:  # Show first 10
                print(f"   - {meal}")
            if len(breakfast_meals) > 10:
                print(f"   ... and {len(breakfast_meals) - 10} more")
        except Exception as e:
            print(f"❌ Error: {e}")

        # Test 2: Find all vegan meals
        print("\n2. All vegan meals:")
        try:
            results = list(self.prolog.query("meal(Name, _, _, vegan, _, _, _, _, _, _, _, _, _, _)"))
            vegan_meals = [result['Name'] for result in results]
            print(f"Found {len(vegan_meals)} vegan meals:")
            for meal in vegan_meals[:8]:
                print(f"   - {meal}")
            if len(vegan_meals) > 8:
                print(f"   ... and {len(vegan_meals) - 8} more")
        except Exception as e:
            print(f"❌ Error: {e}")

        # Test 3: Find quick meals (under 15 minutes)
        print("\n3. Quick meals (≤15 minutes):")
        try:
            results = list(self.prolog.query("meal(Name, _, _, _, _, _, _, _, _, _, _, _, Time, _), Time =< 15"))
            quick_meals = [(result['Name'], result['Time']) for result in results]
            print(f"Found {len(quick_meals)} quick meals:")
            for meal, time in quick_meals[:8]:
                print(f"   - {meal}: {time} min")
            if len(quick_meals) > 8:
                print(f"   ... and {len(quick_meals) - 8} more")
        except Exception as e:
            print(f"❌ Error: {e}")

    def test_cuisine_queries(self):
        """Test cuisine-specific queries"""
        print("\n" + "="*50)
        print("TESTING CUISINE QUERIES")
        print("="*50)
        
        cuisines = ['italian', 'asian', 'mexican', 'mediterranean', 'american']
        
        for cuisine in cuisines:
            print(f"\n{cuisine.title()} cuisine meals:")
            try:
                query = f"meal(Name, _, _, _, _, _, _, _, {cuisine}, _, _, _, _, _)"
                results = list(self.prolog.query(query))
                meals = [result['Name'] for result in results]
                print(f"Found {len(meals)} {cuisine} meals:")
                for meal in meals[:5]:
                    print(f"   - {meal}")
                if len(meals) > 5:
                    print(f"   ... and {len(meals) - 5} more")
            except Exception as e:
                print(f"❌ Error: {e}")

    def test_recommendation_system(self):
        """Test the meal recommendation system with various scenarios"""
        print("\n" + "="*50)
        print("TESTING RECOMMENDATION SYSTEM")
        print("="*50)

        # Define test cases
        test_cases = [
            {
                'name': 'Weight Loss Breakfast (Vegetarian, Quick)',
                'query': 'recommend_meal(answers("weight_loss", "breakfast", "vegetarian", "no_preference", "mild", "quick_easy", "no_preference", "savory", "very_healthy", "less_10_min", [], "low"), Meal)'
            },
            {
                'name': 'Muscle Gain Dinner (Chicken, Asian)',
                'query': 'recommend_meal(answers("muscle_gain", "dinner", "no_restrictions", "chicken", "medium", "home_cooked", "asian", "savory", "balanced", "10_30_min", [], "medium"), Meal)'
            },
            {
                'name': 'Vegan Lunch (Plant Protein)',
                'query': 'recommend_meal(answers("general_health", "lunch", "vegan", "plant_based", "mild", "quick_easy", "middle_eastern", "savory", "very_healthy", "less_10_min", [], "low"), Meal)'
            },
            {
                'name': 'Energy Boost Snack (Sweet)',
                'query': 'recommend_meal(answers("energy_boost", "snack", "no_restrictions", "no_preference", "mild", "quick_easy", "american", "sweet", "balanced", "less_10_min", [], "low"), Meal)'
            },
            {
                'name': 'High-end Seafood Dinner',
                'query': 'recommend_meal(answers("tasty_food", "dinner", "no_restrictions", "fish_seafood", "mild", "restaurant_style", "italian", "savory", "balanced", "10_30_min", [], "high"), Meal)'
            },
            {
                'name': 'Spicy Comfort Food',
                'query': 'recommend_meal(answers("energy_boost", "dinner", "gluten_free", "beef", "spicy", "comfort_food", "american", "savory", "treat_day", "more_30_min", [], "low"), Meal)'
            }
        ]

        for i, test_case in enumerate(test_cases, 1):
            print(f"\n{i}. {test_case['name']}:")
            try:
                results = list(self.prolog.query(test_case['query']))
                if results:
                    meals = [result['Meal'] for result in results]
                    print(f"   Recommended meals: {meals}")
                else:
                    print("   ❌ No meals found matching criteria")
            except Exception as e:
                print(f"   ❌ Error: {e}")

    def test_specific_dietary_requirements(self):
        """Test specific dietary requirement scenarios"""
        print("\n" + "="*50)
        print("TESTING DIETARY REQUIREMENTS")
        print("="*50)

        dietary_tests = [
            ('Gluten-free meals', 'meal(Name, _, _, gluten_free, _, _, _, _, _, _, _, _, _, _)'),
            ('Low-carb meals', 'meal(Name, _, _, low_carb, _, _, _, _, _, _, _, _, _, _)'),
            ('High healthy level meals', 'meal(Name, _, _, _, _, _, _, _, _, _, high, _, _, _)'),
            ('Low budget meals', 'meal(Name, _, _, _, _, _, _, _, _, _, _, low, _, _)'),
            ('Sweet meals/desserts', 'meal(Name, _, _, _, _, _, _, _, _, _, _, _, _, sugar)'),
            ('Spicy meals', 'meal(Name, _, _, _, _, Spice, _, _, _, _, _, _, _, _), member(Spice, [medium, hot])')
        ]

        for test_name, query in dietary_tests:
            print(f"\n{test_name}:")
            try:
                results = list(self.prolog.query(query))
                if 'Spice' in query:
                    meals = [(result['Name'], result['Spice']) for result in results]
                    print(f"   Found {len(meals)} meals:")
                    for meal, spice in meals[:6]:
                        print(f"   - {meal} ({spice})")
                else:
                    meals = [result['Name'] for result in results]
                    print(f"   Found {len(meals)} meals:")
                    for meal in meals[:6]:
                        print(f"   - {meal}")
                if len(meals) > 6:
                    print(f"   ... and {len(meals) - 6} more")
            except Exception as e:
                print(f"   ❌ Error: {e}")

    def test_meal_details(self):
        """Test getting detailed information about specific meals"""
        print("\n" + "="*50)
        print("TESTING MEAL DETAILS")
        print("="*50)

        sample_meals = ['egg_omelette', 'chicken_stir_fry', 'greek_salad', 'sushi_roll', 'thai_curry']
        
        for meal in sample_meals:
            print(f"\nDetails for {meal}:")
            try:
                query = f"meal({meal}, MealTime, MealGoal, DietaryType, ProteinType, SpicyLevel, MealStyle, Ingredients, MealCuisine, MealType, HealthyLevel, Budget, PrepTime, SaltOrSugar)"
                results = list(self.prolog.query(query))
                if results:
                    result = results[0]
                    print(f"   Time: {result['MealTime']}")
                    print(f"   Goal: {result['MealGoal']}")
                    print(f"   Diet: {result['DietaryType']}")
                    print(f"   Protein: {result['ProteinType']}")
                    print(f"   Spice: {result['SpicyLevel']}")
                    print(f"   Style: {result['MealStyle']}")
                    print(f"   Cuisine: {result['MealCuisine']}")
                    print(f"   Health: {result['HealthyLevel']}")
                    print(f"   Budget: {result['Budget']}")
                    print(f"   Prep Time: {result['PrepTime']} min")
                    print(f"   Taste: {result['SaltOrSugar']}")
                    print(f"   Ingredients: {result['Ingredients']}")
                else:
                    print(f"   ❌ Meal '{meal}' not found")
            except Exception as e:
                print(f"   ❌ Error: {e}")

    def test_mapping_functions(self):
        """Test the mapping functions"""
        print("\n" + "="*50)
        print("TESTING MAPPING FUNCTIONS")
        print("="*50)

        mapping_tests = [
            ('Goal mapping', 'map_goal("weight_loss", Goal)'),
            ('Time mapping', 'map_time("breakfast", Time)'),
            ('Diet mapping', 'map_diet("vegetarian", Diet)'),
            ('Protein mapping', 'map_protein("chicken", Protein)'),
            ('Spice mapping', 'map_spice("medium", Spice)'),
            ('Style mapping', 'map_style("quick_easy", Style)'),
            ('Cuisine mapping', 'map_cuisine("italian", Cuisine)'),
            ('Taste mapping', 'map_taste("sweet", Taste)'),
            ('Health mapping', 'map_health("very_healthy", Health)'),
            ('Budget mapping', 'map_budget("low", Budget)')
        ]

        for test_name, query in mapping_tests:
            print(f"\n{test_name}:")
            try:
                results = list(self.prolog.query(query))
                if results:
                    for result in results:
                        mapped_value = list(result.values())[0]
                        print(f"   ✅ Mapped to: {mapped_value}")
                else:
                    print("   ❌ No mapping found")
            except Exception as e:
                print(f"   ❌ Error: {e}")

    def run_comprehensive_test(self):
        """Run all tests"""
        print("🍽️  MEAL RECOMMENDATION SYSTEM - COMPREHENSIVE TEST")
        print("=" * 60)
        
        try:
            self.test_basic_meal_queries()
            self.test_cuisine_queries()
            self.test_recommendation_system()
            self.test_specific_dietary_requirements()
            self.test_meal_details()
            self.test_mapping_functions()
            
            print("\n" + "="*60)
            print("✅ ALL TESTS COMPLETED!")
            print("="*60)
            
        except Exception as e:
            print(f"\n❌ Test suite failed with error: {e}")

    def interactive_test(self):
        """Interactive testing mode"""
        print("\n🍽️  INTERACTIVE MEAL RECOMMENDATION")
        print("="*40)
        print("Enter your preferences (or 'quit' to exit):")
        
        while True:
            try:
                # Get user input
                goal = input("\nMeal goal (weight_loss/muscle_gain/energy_boost/general_health/tasty_food): ").strip()
                if goal.lower() == 'quit':
                    break
                    
                time = input("Meal time (breakfast/lunch/dinner/snack): ").strip()
                diet = input("Diet type (vegetarian/vegan/gluten_free/dairy_free/no_restrictions): ").strip()
                protein = input("Protein preference (chicken/beef/fish_seafood/plant_based/no_preference): ").strip()
                spice = input("Spice level (mild/medium/spicy/very_spicy): ").strip()
                
                # Build and execute query
                query = f'recommend_meal(answers("{goal}", "{time}", "{diet}", "{protein}", "{spice}", "quick_easy", "no_preference", "savory", "balanced", "10_30_min", [], "medium"), Meal)'
                
                results = list(self.prolog.query(query))
                if results:
                    meals = [result['Meal'] for result in results]
                    print(query)
                    print(f"\n🍽️  Recommended meals: {meals}")
                else:
                    print("\n❌ No meals found matching your criteria")
                    
            except KeyboardInterrupt:
                print("\n👋 Goodbye!")
                break
            except Exception as e:
                print(f"❌ Error: {e}")

def main():
    """Main function to run the tests"""
    print("PySwip Meal Recommendation System Tester")
    print("=" * 50)
    
    # You'll need to update this path to your Prolog file
    prolog_file = "data2.pl"  # Update this path!
    
    try:
        tester = MealRecommendationTester(prolog_file)
        
        print("\nSelect test mode:")
        print("1. Run comprehensive test suite")
        print("2. Interactive recommendation test")
        print("3. Both")
        
        choice = input("\nEnter choice (1/2/3): ").strip()
        
        if choice == "1":
            tester.run_comprehensive_test()
        elif choice == "2":
            tester.interactive_test()
        elif choice == "3":
            tester.run_comprehensive_test()
            tester.interactive_test()
        else:
            print("Invalid choice. Running comprehensive test...")
            tester.run_comprehensive_test()
            
    except Exception as e:
        print(f"❌ Failed to initialize tester: {e}")
        print("\nMake sure:")
        print("1. PySwip is installed: pip install pyswip")
        print("2. SWI-Prolog is installed on your system")
        print("3. The Prolog file path is correct")

if __name__ == "__main__":
    main()