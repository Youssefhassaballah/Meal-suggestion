// components/Questions.js
import React from 'react';

export const Question1 = ({ answer, onAnswer }) => (
  <div className="space-y-6">
    <div className="text-center">
      <div className="text-6xl mb-4">🥗</div>
      <h2 className="text-2xl font-bold text-gray-800 mb-2">What is your primary goal for this meal?</h2>
    </div>
    <div className="space-y-3">
      {[
        { value: 'weight_loss', label: 'Weight loss' },
        { value: 'muscle_gain', label: 'Muscle gain' },
        { value: 'energy_boost', label: 'Energy boost' },
        { value: 'general_health', label: 'General health' },
        { value: 'tasty_food', label: 'Just tasty food' }
      ].map((option) => (
        <label key={option.value} className="flex items-center p-4 border rounded-lg hover:bg-blue-50 cursor-pointer transition-colors">
          <input
            type="radio"
            name="goal"
            value={option.value}
            checked={answer === option.value}
            onChange={(e) => onAnswer(e.target.value)}
            className="mr-3 text-blue-600"
          />
          <span className="text-gray-700">{option.label}</span>
        </label>
      ))}
    </div>
  </div>
);

export const Question2 = ({ answer, onAnswer }) => (
  <div className="space-y-6">
    <div className="text-center">
      <div className="text-6xl mb-4">🕒</div>
      <h2 className="text-2xl font-bold text-gray-800 mb-2">What time of day is it?</h2>
    </div>
    <div className="space-y-3">
      {[
        { value: 'breakfast', label: 'Breakfast' },
        { value: 'lunch', label: 'Lunch' },
        { value: 'dinner', label: 'Dinner' },
        { value: 'snack', label: 'Snack' }
      ].map((option) => (
        <label key={option.value} className="flex items-center p-4 border rounded-lg hover:bg-blue-50 cursor-pointer transition-colors">
          <input
            type="radio"
            name="time"
            value={option.value}
            checked={answer === option.value}
            onChange={(e) => onAnswer(e.target.value)}
            className="mr-3 text-blue-600"
          />
          <span className="text-gray-700">{option.label}</span>
        </label>
      ))}
    </div>
  </div>
);

export const Question3 = ({ answer, onAnswer }) => (
  <div className="space-y-6">
    <div className="text-center">
      <div className="text-6xl mb-4">🌱</div>
      <h2 className="text-2xl font-bold text-gray-800 mb-2">Do you follow any dietary restrictions?</h2>
    </div>
    <div className="space-y-3">
      {[
        { value: 'vegetarian', label: 'Vegetarian' },
        { value: 'vegan', label: 'Vegan' },
        { value: 'gluten_free', label: 'Gluten-free' },
        { value: 'dairy_free', label: 'Dairy-free' },
        { value: 'no_restrictions', label: 'No restrictions' }
      ].map((option) => (
        <label key={option.value} className="flex items-center p-4 border rounded-lg hover:bg-blue-50 cursor-pointer transition-colors">
          <input
            type="radio"
            name="dietary"
            value={option.value}
            checked={answer === option.value}
            onChange={(e) => onAnswer(e.target.value)}
            className="mr-3 text-blue-600"
          />
          <span className="text-gray-700">{option.label}</span>
        </label>
      ))}
    </div>
  </div>
);

export const Question4 = ({ answer, onAnswer }) => (
  <div className="space-y-6">
    <div className="text-center">
      <div className="text-6xl mb-4">🐔</div>
      <h2 className="text-2xl font-bold text-gray-800 mb-2">What type of protein do you prefer?</h2>
    </div>
    <div className="space-y-3">
      {[
        { value: 'chicken', label: 'Chicken' },
        { value: 'beef', label: 'Beef' },
        { value: 'fish_seafood', label: 'Fish/Seafood' },
        { value: 'plant_based', label: 'Plant-based (e.g., tofu, beans)' },
        { value: 'no_preference', label: 'No preference' }
      ].map((option) => (
        <label key={option.value} className="flex items-center p-4 border rounded-lg hover:bg-blue-50 cursor-pointer transition-colors">
          <input
            type="radio"
            name="protein"
            value={option.value}
            checked={answer === option.value}
            onChange={(e) => onAnswer(e.target.value)}
            className="mr-3 text-blue-600"
          />
          <span className="text-gray-700">{option.label}</span>
        </label>
      ))}
    </div>
  </div>
);

export const Question5 = ({ answer, onAnswer }) => (
  <div className="space-y-6">
    <div className="text-center">
      <div className="text-6xl mb-4">🌶️</div>
      <h2 className="text-2xl font-bold text-gray-800 mb-2">How spicy do you like your food?</h2>
    </div>
    <div className="space-y-3">
      {[
        { value: 'mild', label: 'Mild' },
        { value: 'medium', label: 'Medium' },
        { value: 'spicy', label: 'Spicy' },
        { value: 'very_spicy', label: 'Very Spicy' }
      ].map((option) => (
        <label key={option.value} className="flex items-center p-4 border rounded-lg hover:bg-blue-50 cursor-pointer transition-colors">
          <input
            type="radio"
            name="spice"
            value={option.value}
            checked={answer === option.value}
            onChange={(e) => onAnswer(e.target.value)}
            className="mr-3 text-blue-600"
          />
          <span className="text-gray-700">{option.label}</span>
        </label>
      ))}
    </div>
  </div>
);

export const Question6 = ({ answer, onAnswer }) => (
  <div className="space-y-6">
    <div className="text-center">
      <div className="text-6xl mb-4">🥖</div>
      <h2 className="text-2xl font-bold text-gray-800 mb-2">What kind of meal are you craving?</h2>
    </div>
    <div className="space-y-3">
      {[
        { value: 'quick_easy', label: 'Something quick and easy' },
        { value: 'home_cooked', label: 'A home-cooked dish' },
        { value: 'restaurant_style', label: 'A fancy restaurant-style plate' },
        { value: 'comfort_food', label: 'Comfort food' }
      ].map((option) => (
        <label key={option.value} className="flex items-center p-4 border rounded-lg hover:bg-blue-50 cursor-pointer transition-colors">
          <input
            type="radio"
            name="meal_type"
            value={option.value}
            checked={answer === option.value}
            onChange={(e) => onAnswer(e.target.value)}
            className="mr-3 text-blue-600"
          />
          <span className="text-gray-700">{option.label}</span>
        </label>
      ))}
    </div>
  </div>
);

export const Question7 = ({ answer, onAnswer }) => (
  <div className="space-y-6">
    <div className="text-center">
      <div className="text-6xl mb-4">🌍</div>
      <h2 className="text-2xl font-bold text-gray-800 mb-2">Do you prefer a certain cuisine?</h2>
    </div>
    <div className="space-y-3">
      {[
        { value: 'middle_eastern', label: 'Middle Eastern' },
        { value: 'italian', label: 'Italian' },
        { value: 'asian', label: 'Asian' },
        { value: 'american', label: 'American' },
        { value: 'no_preference', label: 'No preference' }
      ].map((option) => (
        <label key={option.value} className="flex items-center p-4 border rounded-lg hover:bg-blue-50 cursor-pointer transition-colors">
          <input
            type="radio"
            name="cuisine"
            value={option.value}
            checked={answer === option.value}
            onChange={(e) => onAnswer(e.target.value)}
            className="mr-3 text-blue-600"
          />
          <span className="text-gray-700">{option.label}</span>
        </label>
      ))}
    </div>
  </div>
);

export const Question8 = ({ answer, onAnswer }) => (
  <div className="space-y-6">
    <div className="text-center">
      <div className="text-6xl mb-4">🧁</div>
      <h2 className="text-2xl font-bold text-gray-800 mb-2">Do you want a sweet or savory dish?</h2>
    </div>
    <div className="space-y-3">
      {[
        { value: 'sweet', label: 'Sweet' },
        { value: 'savory', label: 'Savory' },
        { value: 'mix_both', label: 'A mix of both' }
      ].map((option) => (
        <label key={option.value} className="flex items-center p-4 border rounded-lg hover:bg-blue-50 cursor-pointer transition-colors">
          <input
            type="radio"
            name="taste"
            value={option.value}
            checked={answer === option.value}
            onChange={(e) => onAnswer(e.target.value)}
            className="mr-3 text-blue-600"
          />
          <span className="text-gray-700">{option.label}</span>
        </label>
      ))}
    </div>
  </div>
);

export const Question9 = ({ answer, onAnswer }) => (
  <div className="space-y-6">
    <div className="text-center">
      <div className="text-6xl mb-4">🥦</div>
      <h2 className="text-2xl font-bold text-gray-800 mb-2">How healthy do you want your meal to be?</h2>
    </div>
    <div className="space-y-3">
      {[
        { value: 'very_healthy', label: 'Very healthy (low fat, low sugar)' },
        { value: 'balanced', label: 'Balanced' },
        { value: 'treat_day', label: "Doesn't matter – treat day!" }
      ].map((option) => (
        <label key={option.value} className="flex items-center p-4 border rounded-lg hover:bg-blue-50 cursor-pointer transition-colors">
          <input
            type="radio"
            name="health"
            value={option.value}
            checked={answer === option.value}
            onChange={(e) => onAnswer(e.target.value)}
            className="mr-3 text-blue-600"
          />
          <span className="text-gray-700">{option.label}</span>
        </label>
      ))}
    </div>
  </div>
);

export const Question10 = ({ answer, onAnswer }) => (
  <div className="space-y-6">
    <div className="text-center">
      <div className="text-6xl mb-4">⏱️</div>
      <h2 className="text-2xl font-bold text-gray-800 mb-2">How much time do you have to prepare the meal?</h2>
    </div>
    <div className="space-y-3">
      {[
        { value: 'less_10_min', label: 'Less than 10 minutes' },
        { value: '10_30_min', label: '10–30 minutes' },
        { value: 'more_30_min', label: 'More than 30 minutes' },
        { value: 'no_cooking', label: 'I prefer no cooking – just ready-to-eat' }
      ].map((option) => (
        <label key={option.value} className="flex items-center p-4 border rounded-lg hover:bg-blue-50 cursor-pointer transition-colors">
          <input
            type="radio"
            name="prep_time"
            value={option.value}
            checked={answer === option.value}
            onChange={(e) => onAnswer(e.target.value)}
            className="mr-3 text-blue-600"
          />
          <span className="text-gray-700">{option.label}</span>
        </label>
      ))}
    </div>
  </div>
);
export const Question11 = ({ answer, onAnswer }) => (
  <div className="space-y-6">
    <div className="text-center">
      <div className="text-6xl mb-4">🚫</div>
      <h2 className="text-2xl font-bold text-gray-800 mb-2">Are there any ingredients you want to avoid?</h2>
    </div>
    <div>
      <textarea
        value={answer}
        onChange={(e) => onAnswer(e.target.value)}
        placeholder="E.g., mushrooms, peanuts, onions..."
        className="w-full p-4 border rounded-lg text-gray-700 focus:outline-none focus:ring-2 focus:ring-blue-400"
        rows={3}
      />
    </div>
  </div>
);

export const Question12 = ({ answer, onAnswer }) => (
  <div className="space-y-6">
    <div className="text-center">
      <div className="text-6xl mb-4">💰</div>
      <h2 className="text-2xl font-bold text-gray-800 mb-2">What’s your budget for this meal?</h2>
    </div>
    <div className="space-y-3">
      {[
        { value: 'low', label: 'Budget-friendly' },
        { value: 'medium', label: 'Moderate' },
        { value: 'high', label: 'Willing to splurge' }
      ].map((option) => (
        <label key={option.value} className="flex items-center p-4 border rounded-lg hover:bg-blue-50 cursor-pointer transition-colors">
          <input
            type="radio"
            name="budget"
            value={option.value}
            checked={answer === option.value}
            onChange={(e) => onAnswer(e.target.value)}
            className="mr-3 text-blue-600"
          />
          <span className="text-gray-700">{option.label}</span>
        </label>
      ))}
    </div>
  </div>
);
