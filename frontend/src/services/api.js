// services/api.js

const API_BASE_URL = process.env.REACT_APP_API_URL || 'http://172.21.203.231:5000';

export const submitMealQuestionnaire = async (answers) => {
  try {
    const response = await fetch(`${API_BASE_URL}/meal_suggestion`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        // Add any auth headers if needed
        // 'Authorization': `Bearer ${token}`,
      },
      body: JSON.stringify({
        answers: answers,
        timestamp: new Date().toISOString()
        // Add any additional metadata
      
       
      })
    });

    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }

    const data = await response.json();
    return data;
  } catch (error) {
    console.error('Error submitting questionnaire:', error);
    throw error;
  }
};


// Additional API functions you might need
export const getMealRecommendations = async (userId) => {
  try {
    const response = await fetch(`${API_BASE_URL}/api/meal-recommendations/${userId}`);
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    return await response.json();
  } catch (error) {
    console.error('Error fetching meal recommendations:', error);
    throw error;
  }
};

export const getUserPreferences = async (userId) => {
  try {
    const response = await fetch(`${API_BASE_URL}/api/user-preferences/${userId}`);
    if (!response.ok) {
      throw new Error(`HTTP error! status: ${response.status}`);
    }
    return await response.json();
  } catch (error) {
    console.error('Error fetching user preferences:', error);
    throw error;
  }
};