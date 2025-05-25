// components/Navigation.js
import React from 'react';
import { ChevronLeft, ChevronRight, Send } from 'lucide-react';

const Navigation = ({ 
  currentQuestion, 
  totalQuestions, 
  isAnswered, 
  isSubmitting, 
  onPrevious, 
  onNext, 
  onSubmit 
}) => {
  const isLastQuestion = currentQuestion === totalQuestions - 1;

  return (
    <div className="flex justify-between items-center">
      <button
        onClick={onPrevious}
        disabled={currentQuestion === 0}
        className="flex items-center px-4 py-2 text-gray-600 disabled:text-gray-400 disabled:cursor-not-allowed hover:text-gray-800 transition-colors"
      >
        <ChevronLeft className="mr-1" size={20} />
        Previous
      </button>

      {isLastQuestion ? (
        <button
          onClick={onSubmit}
          disabled={!isAnswered || isSubmitting}
          className="flex items-center px-6 py-2 bg-green-600 text-white rounded-lg disabled:bg-gray-400 disabled:cursor-not-allowed hover:bg-green-700 transition-colors"
        >
          {isSubmitting ? (
            <>
              <div className="animate-spin rounded-full h-4 w-4 border-2 border-white border-t-transparent mr-2"></div>
              Submitting...
            </>
          ) : (
            <>
              <Send className="mr-2" size={16} />
              Submit
            </>
          )}
        </button>
      ) : (
        <button
          onClick={onNext}
          disabled={!isAnswered}
          className="flex items-center px-4 py-2 bg-blue-600 text-white rounded-lg disabled:bg-gray-400 disabled:cursor-not-allowed hover:bg-blue-700 transition-colors"
        >
          Next
          <ChevronRight className="ml-1" size={20} />
        </button>
      )}
    </div>
  );
};

export default Navigation;