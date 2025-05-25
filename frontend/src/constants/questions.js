// constants/questions.js

export const QUESTION_KEYS = [
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
  'avoided_ingredients',
  'budget'
];

export const QUESTION_TITLES = [
  'What is your primary goal for this meal?',
  'What time of day is it?',
  'Do you follow any dietary restrictions?',
  'What type of protein do you prefer?',
  'How spicy do you like your food?',
  'What kind of meal are you craving?',
  'Do you prefer a certain cuisine?',
  'Do you want a sweet or savory dish?',
  'How healthy do you want your meal to be?',
  'How much time do you have to prepare the meal?'
];

export const QUESTION_OPTIONS = {
  goal: [
    { value: 'weight_loss', label: 'Weight loss' },
    { value: 'muscle_gain', label: 'Muscle gain' },
    { value: 'energy_boost', label: 'Energy boost' },
    { value: 'general_health', label: 'General health' },
    { value: 'tasty_food', label: 'Just tasty food' }
  ],
  time: [
    { value: 'breakfast', label: 'Breakfast' },
    { value: 'lunch', label: 'Lunch' },
    { value: 'dinner', label: 'Dinner' },
    { value: 'snack', label: 'Snack' }
  ],
  dietary: [
    { value: 'vegetarian', label: 'Vegetarian' },
    { value: 'vegan', label: 'Vegan' },
    { value: 'gluten_free', label: 'Gluten-free' },
    { value: 'dairy_free', label: 'Dairy-free' },
    { value: 'no_restrictions', label: 'No restrictions' }
  ],
  protein: [
    { value: 'chicken', label: 'Chicken' },
    { value: 'beef', label: 'Beef' },
    { value: 'fish_seafood', label: 'Fish/Seafood' },
    { value: 'plant_based', label: 'Plant-based (e.g., tofu, beans)' },
    { value: 'no_preference', label: 'No preference' }
  ],
  spice: [
    { value: 'mild', label: 'Mild' },
    { value: 'medium', label: 'Medium' },
    { value: 'spicy', label: 'Spicy' },
    { value: 'very_spicy', label: 'Very Spicy' }
  ],
  meal_type: [
    { value: 'quick_easy', label: 'Something quick and easy' },
    { value: 'home_cooked', label: 'A home-cooked dish' },
    { value: 'restaurant_style', label: 'A fancy restaurant-style plate' },
    { value: 'comfort_food', label: 'Comfort food' }
  ],
  cuisine: [
    { value: 'middle_eastern', label: 'Middle Eastern' },
    { value: 'italian', label: 'Italian' },
    { value: 'asian', label: 'Asian' },
    { value: 'american', label: 'American' },
    { value: 'no_preference', label: 'No preference' }
  ],
  taste: [
    { value: 'sweet', label: 'Sweet' },
    { value: 'savory', label: 'Savory' },
    { value: 'mix_both', label: 'A mix of both' }
  ],
  health: [
    { value: 'very_healthy', label: 'Very healthy (low fat, low sugar)' },
    { value: 'balanced', label: 'Balanced' },
    { value: 'treat_day', label: "Doesn't matter – treat day!" }
  ],
  prep_time: [
    { value: 'less_10_min', label: 'Less than 10 minutes' },
    { value: '10_30_min', label: '10–30 minutes' },
    { value: 'more_30_min', label: 'More than 30 minutes' },
    { value: 'no_cooking', label: 'I prefer no cooking – just ready-to-eat' }
  ],
  budget: [
    { value: 'low', label: 'Low budget' },
    { value: 'medium', label: 'Medium budget' },
    { value: 'high', label: 'High budget' }
  ],
  avoided_ingredients: [
    
  ]
};

export const QUESTION_EMOJIS = [
  '🥗', '🕒', '🌱', '🐔', '🌶️',
  '🥖', '🌍', '🧁', '🥦', '⏱️'
];