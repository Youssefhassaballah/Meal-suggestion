from flask  import Flask, request, jsonify
from flask_cors import CORS
from meal_planner import MealPlanner  # Assuming you have a MealPlanner class in meal_planner.py

app = Flask(__name__)
CORS(app)
name = ""
meal_goal = ""
type = ""
spicy_level = ""
meal_type = ""


@app.route('/start', methods=['POST'])
def start():
    data = request.get_json()
    if not data or 'name' not in data:
        return jsonify({"error": "Name is required"}), 400
    
    name = data['name']
    return jsonify({"message": f"Hello, {name}!"}), 200

@app.route('/meal_suggestion', methods=['POST'])
def meal_suggestion():
    data = request.get_json()
    if not data or 'answers' not in data:
        return jsonify({"error": "Answers are required"}), 400

    answers = data['answers']
    meal_planner = MealPlanner(answers)
    response = meal_planner.suggest_meal()
    if not response:
        return jsonify({"error": "No meal suggestions found"}), 404
    print(f"Meal suggestion response: {response}")
    print(f"")

    print(f"Received answers abdo : {answers}")
    # Here you would typically call your meal planning logic
    
    return jsonify({"message": f"Meal suggestion based on {response}!"}), 200

if __name__ == "__main__":
    app.run(host="0.0.0.0", port=5000,debug=True)