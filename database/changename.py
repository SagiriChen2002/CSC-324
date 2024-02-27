import pandas as pd

# Mapping of US state names to abbreviations
state_abbreviations = {
    'Alabama': 'AL', 'Alaska': 'AK', 'Arizona': 'AZ', 'Arkansas': 'AR', 'California': 'CA',
    'Colorado': 'CO', 'Connecticut': 'CT', 'Delaware': 'DE', 'Florida': 'FL', 'Georgia': 'GA',
    'Hawaii': 'HI', 'Idaho': 'ID', 'Illinois': 'IL', 'Indiana': 'IN', 'Iowa': 'IA',
    'Kansas': 'KS', 'Kentucky': 'KY', 'Louisiana': 'LA', 'Maine': 'ME', 'Maryland': 'MD',
    'Massachusetts': 'MA', 'Michigan': 'MI', 'Minnesota': 'MN', 'Mississippi': 'MS',
    'Missouri': 'MO', 'Montana': 'MT', 'Nebraska': 'NE', 'Nevada': 'NV', 'New Hampshire': 'NH',
    'New Jersey': 'NJ', 'New Mexico': 'NM', 'New York': 'NY', 'North Carolina': 'NC',
    'North Dakota': 'ND', 'Ohio': 'OH', 'Oklahoma': 'OK', 'Oregon': 'OR', 'Pennsylvania': 'PA',
    'Rhode Island': 'RI', 'South Carolina': 'SC', 'South Dakota': 'SD', 'Tennessee': 'TN',
    'Texas': 'TX', 'Utah': 'UT', 'Vermont': 'VT', 'Virginia': 'VA', 'Washington': 'WA',
    'West Virginia': 'WV', 'Wisconsin': 'WI', 'Wyoming': 'WY', 'District of Columbia': 'DC'
}


def convert_state_names(input_file, output_file):
    # Read the input CSV file
    data = pd.read_csv(input_file)

    # Check if the 'State' column exists
    if 'state' in data.columns:
        # Replace full state names with abbreviations
        data['state'] = data['state'].map(state_abbreviations).fillna(data['state'])

        # Save the modified data to the output CSV file
        data.to_csv(output_file, index=False)

        print("Conversion complete. Output saved to:", output_file)
    else:
        print("Error: 'State' column not found in the input file.")


# Example usage of the function
# Replace 'path/to/your_input.csv' with the actual path to your CSV file
# And 'path/to/your_output.csv' with the desired output file path
convert_state_names('database/us-states.csv', 'database/us-states-output.csv')

# %%
