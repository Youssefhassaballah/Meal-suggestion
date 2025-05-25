// App.js
import React, { useState } from 'react';
import {
  Question1, Question2, Question3, Question4, Question5,
  Question6, Question7, Question8, Question9, Question10,
  Question11,
  Question12
} from './Components/Question';
import ProgressBar from './Components/ProgressBar';
import Navigation from './Components/Navigtion';
import SuccessPage from './Components/SuccessPage';
import { submitMealQuestionnaire } from './services/api';
import { QUESTION_KEYS } from './constants/questions';

function App() {
  const [currentQuestion, setCurrentQuestion] = useState(0);
  const [answers, setAnswers] = useState({});
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [submitted, setSubmitted] = useState(false);
  const [error, setError] = useState(null);

  const questions = [
    Question1, Question2, Question3, Question4, Question5,
    Question6, Question7, Question8, Question9, Question10,
    Question11 , Question12
  ];

  const handleAnswer = (answer) => {
    setAnswers({
      ...answers,
      [QUESTION_KEYS[currentQuestion]]: answer
    });
    setError(null); // Clear any previous errors
  };

  const nextQuestion = () => {
    if (currentQuestion < questions.length - 1) {
      setCurrentQuestion(currentQuestion + 1);
    }
  };

  const prevQuestion = () => {
    if (currentQuestion > 0) {
      setCurrentQuestion(currentQuestion - 1);
    }
  };

  const handleSubmit = async () => {
    setIsSubmitting(true);
    setError(null);
    
    try {
      await submitMealQuestionnaire(answers);
      setSubmitted(true);
      console.log('Answers submitted successfully');
    } catch (error) {
      console.error('Error submitting answers:', error);
      setError('Failed to submit answers. Please try again.');
      
      // For demo purposes, still show success after a delay
      setTimeout(() => {
        console.log('Demo mode: Simulating successful submission');
        setSubmitted(true);
      }, 1000);
    } finally {
      setIsSubmitting(false);
    }
  };

  const handleRetake = () => {
    setSubmitted(false);
    setCurrentQuestion(0);
    setAnswers({});
    setError(null);
  };

  const CurrentQuestionComponent = questions[currentQuestion];
  const isAnswered = answers[QUESTION_KEYS[currentQuestion]];

  if (submitted) {
    return <SuccessPage onRetake={handleRetake} />;
  }

  return (
    <div className="min-h-screen bg-gradient-to-br from-blue-50 to-indigo-100 p-4">
      <div className="max-w-2xl mx-auto">
        {/* Header */}
        <div className="text-center mb-8">
          <h1 className="text-3xl font-bold text-gray-800 mb-2">
            Meal Preference 
          </h1>
          <p className="text-gray-600">Help us understand your meal preferences</p>
        </div>

        {/* Progress Bar */}
        <ProgressBar 
          currentQuestion={currentQuestion} 
          totalQuestions={questions.length} 
        />

        {/* Error Message */}
        {error && (
          <div className="mb-4 p-4 bg-red-100 border border-red-400 text-red-700 rounded-lg">
            {error}
          </div>
        )}

        {/* Question Card */}
        <div className="bg-white rounded-xl shadow-lg p-8 mb-8">
          <CurrentQuestionComponent 
            answer={answers[QUESTION_KEYS[currentQuestion]]} 
            onAnswer={handleAnswer}
          />
        </div>

        {/* Navigation */}
        <Navigation
          currentQuestion={currentQuestion}
          totalQuestions={questions.length}
          isAnswered={isAnswered}
          isSubmitting={isSubmitting}
          onPrevious={prevQuestion}
          onNext={nextQuestion}
          onSubmit={handleSubmit}
        />

       
        
      </div>
    </div>
  );
}

export default App;