// components/SuccessPage.js
import React from 'react';

const SuccessPage = ({ onRetake }) => {
  return (
    <div className="min-h-screen bg-gradient-to-br from-blue-50 to-indigo-100 flex items-center justify-center p-4">
      <div className="bg-white rounded-xl shadow-lg p-8 max-w-md w-full text-center">
        <div className="text-6xl mb-4">🎉</div>
        <h2 className="text-2xl font-bold text-gray-800 mb-4">Thank You!</h2>
        <p className="text-gray-600 mb-6">
          Your meal preferences have been submitted successfully. We'll use this 
          information to provide you with personalized meal recommendations.
        </p>
        <button
          onClick={onRetake}
          className="bg-blue-600 text-white px-6 py-2 rounded-lg hover:bg-blue-700 transition-colors"
        >
          Take Quiz Again
        </button>
      </div>
    </div>
  );
};

export default SuccessPage;