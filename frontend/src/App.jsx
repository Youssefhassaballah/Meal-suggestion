import React, { useState } from "react";
import LinearProgrammingPage from "./pages/linearProgramming";
import GoalProgrammingPage from "./pages/goalProgramming";

const App = () => {
  const [currentTab, setCurrentTab] = useState("Linear Programming");

  const renderContent = () => {
    switch (currentTab) {
      case "Linear Programming":
        return <LinearProgrammingPage />;
      case "Goal Programming":
        return <GoalProgrammingPage />;
      default:
        return <LinearProgrammingPage />;
    }
  };

  return (
    <div className=" bg-gradient-to-br from-indigo-50 to-blue-100">
      <div className="flex justify-center mb-4 h-8">
        <button
          className={`py-2 text-sm font-bold ${
            currentTab === "Linear Programming"
              ? "bg-indigo-700 text-white w-full"
              : "from-indigo-50 to-blue-100 text-gray-700 w-full"
          }`}
          onClick={() => setCurrentTab("Linear Programming")}
        >
          Linear Programming
        </button>
        <button
          className={` py-2  text-sm font-bold ${
            currentTab === "Goal Programming"
              ? "bg-indigo-700 text-white w-full"
              : "from-indigo-50 to-blue-100 text-gray-700 w-full"
          }`}
          onClick={() => setCurrentTab("Goal Programming")}
        >
          Goal Programming
        </button>
      </div>
      <div>{renderContent()}</div>
    </div>
  );
};

export default App;
