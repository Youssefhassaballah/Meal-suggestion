// Components/SuccessPage.js
import React, { useState } from 'react';
import { getMealIngredients } from '../services/api';

const SuccessPage = ({ onRetake, recommendedMeals = [] }) => {
  const [selectedMeal, setSelectedMeal] = useState(null);
  const [ingredients, setIngredients] = useState([]);
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState(null);

  // Use passed recommendedMeals or fallback to mock data
  const mealsToShow = recommendedMeals.length > 0 ? recommendedMeals : [
    { name: "Sorry there no meal available plz make the quiz", score:0 },

  ];

  const handleMealClick = async (mealName) => {
    setIsLoading(true);
    setError(null);
    setSelectedMeal(mealName);

    try {
      const data = await getMealIngredients(mealName);
      setIngredients(data.ingredients || []);
    } catch (error) {
      console.error('Error fetching ingredients:', error);
      setError('Failed to fetch ingredients. Please try again.');
      
      // Mock ingredients for demo purposes
      const mockIngredients = getMockIngredients(mealName);
      setIngredients(mockIngredients);
    } finally {
      setIsLoading(false);
    }
  };

  // Mock function to simulate ingredient data (remove in production)
  const getMockIngredients = (mealName) => {
    const mockData = {
      "Teriyaki Chicken": [
        "2 chicken breasts",
        "1/4 cup teriyaki sauce",
        "2 tbsp soy sauce",
        "1 tbsp honey",
        "2 cloves garlic",
        "1 tsp ginger",
        "2 cups steamed rice",
        "1 cup broccoli"
      ],
      "Beef Stir Fry": [
        "1 lb beef sirloin",
        "2 cups mixed vegetables",
        "3 tbsp vegetable oil",
        "2 tbsp soy sauce",
        "1 tbsp oyster sauce",
        "2 cloves garlic",
        "1 tsp ginger",
        "2 cups cooked rice"
      ],
      "Pork Chops with Apple Sauce": [
        "4 pork chops",
        "2 apples",
        "1/4 cup apple cider",
        "2 tbsp brown sugar",
        "1 tsp cinnamon",
        "Salt and pepper",
        "2 tbsp olive oil",
        "Fresh thyme"
      ],
      "Cobb Salad": [
        "6 cups mixed greens",
        "2 grilled chicken breasts",
        "4 strips bacon",
        "2 hard-boiled eggs",
        "1 avocado",
        "1/2 cup blue cheese",
        "2 tomatoes",
        "Ranch dressing"
      ],
      "Beef Tacos": [
        "1 lb ground beef",
        "8 taco shells",
        "1 packet taco seasoning",
        "1 cup shredded cheese",
        "2 tomatoes",
        "1 head lettuce",
        "1 onion",
        "Sour cream"
      ],
      "Chocolate Protein Pancakes": [
        "2 scoops protein powder",
        "2 eggs",
        "1/2 cup oats",
        "1 banana",
        "2 tbsp cocoa powder",
        "1 tsp baking powder",
        "1/4 cup almond milk",
        "Maple syrup"
      ]
    };
    return mockData[mealName] || ["Ingredients not available"];
  };

  const getScoreColor = (score) => {
    if (score >= 80) return "text-green-600 bg-green-100";
    if (score >= 70) return "text-yellow-600 bg-yellow-100";
    return "text-orange-600 bg-orange-100";
  };

  return (
    <div className="min-h-screen bg-gradient-to-br from-blue-50 to-indigo-100 p-4">
      <div className="max-w-4xl mx-auto">
        {/* Success Header */}
        <div className="text-center mb-8">
          <div className="w-16 h-16 bg-green-100 rounded-full flex items-center justify-center mx-auto mb-4">
            <svg className="w-8 h-8 text-green-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth="2" d="M5 13l4 4L19 7"></path>
            </svg>
          </div>
          <h1 className="text-3xl font-bold text-gray-800 mb-2">Thank You!</h1>
          <p className="text-gray-600 mb-6">
            Your meal preferences have been submitted successfully. 
            We'll use this information to provide you with personalized meal recommendations.
          </p>
        </div>

        {/* Recommended Meals Section */}
        <div className="bg-white rounded-xl shadow-lg p-8 mb-8">
          <h2 className="text-2xl font-bold text-gray-800 mb-6">Recommended Meals for You</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4 mb-6">
            {mealsToShow.map((meal, index) => (
              <button
                key={index}
                onClick={() => handleMealClick(meal.name)}
                className="p-4 border-2 border-gray-200 rounded-lg hover:border-blue-500 hover:bg-blue-50 transition-all duration-200 text-left"
              >
                <div className="flex justify-between items-start mb-2">
                  <h3 className="font-semibold text-gray-800 text-sm">{meal.name}</h3>
                  <span className={`px-2 py-1 rounded-full text-xs font-medium ${getScoreColor(meal.score)}`}>
                    {meal.score}%
                  </span>
                </div>
                <p className="text-xs text-gray-600">Click to view ingredients</p>
              </button>
            ))}
          </div>
        </div>

        {/* Ingredients Section */}
        {selectedMeal && (
          <div className="bg-white rounded-xl shadow-lg p-8 mb-8">
            <h3 className="text-xl font-bold text-gray-800 mb-4">
              Ingredients for {selectedMeal}
            </h3>
            
            {isLoading && (
              <div className="flex items-center justify-center py-8">
                <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600"></div>
                <span className="ml-2 text-gray-600">Loading ingredients...</span>
              </div>
            )}

            {error && (
              <div className="p-4 bg-red-100 border border-red-400 text-red-700 rounded-lg mb-4">
                {error}
              </div>
            )}

            {!isLoading && ingredients.length > 0 && (
              <div className="grid grid-cols-1 md:grid-cols-2 gap-2">
                {ingredients.map((ingredient, index) => (
                  <div key={index} className="flex items-center p-2 bg-gray-50 rounded-lg">
                    <span className="w-2 h-2 bg-blue-500 rounded-full mr-3"></span>
                    <span className="text-gray-700">{ingredient}</span>
                  </div>
                ))}
              </div>
            )}
          </div>
        )}

        {/* Action Buttons */}
        <div className="text-center">
          <button
            onClick={onRetake}
            className="bg-blue-600 hover:bg-blue-700 text-white font-semibold py-3 px-8 rounded-lg transition-colors duration-200"
          >
            Take Quiz Again
          </button>
        </div>
      </div>
    </div>
  );
};

export default SuccessPage;