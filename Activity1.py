import pandas as pd
import numpy as np
from datetime import datetime

# Load the dataset
df = pd.read_csv('SolarFlareData/RHESSI_SolarFlares_SolarCycle24_Task3_2025.csv', delimiter=';')

# Activity 1a: Remove rows with zero position and energy range equal to 3-6 keV
df_cleaned = df[~((df['x_pos'] == 0) & (df['y_pos'] == 0) & (df['energy_band'] == '3-6'))]

# Activity 1b: Median duration in seconds of each solar flare class
print("1b. Median duration by solar flare class (seconds):")
duration_by_class = df_cleaned.groupby('class_str')['duration'].median().sort_values(ascending=False)

for class_name, duration in duration_by_class.items():
    print(f"{class_name}: {duration:.2f} seconds")

# Activity 1c: Average number of total counts of each solar flare class
print("\n" + "="*60)
print("1c. Average total counts by solar flare class:")
avg_counts_by_class = df_cleaned.groupby('class_str')['total_counts'].mean().sort_values(ascending=False)

for class_name, avg_count in avg_counts_by_class.items():
    print(f"{class_name}: {avg_count:,.2f}")

# Activity 1d: Average number of total counts yearly and most active year(s)
print("\n" + "="*60)
print("1d. Yearly average total counts and most active years:")

# Extract year from date
df_cleaned.loc[:, 'year'] = pd.to_datetime(df_cleaned['date'], format='%d-%b-%y').dt.year
# Calculate average total counts by year
yearly_avg_counts = df_cleaned.groupby('year')['total_counts'].mean().sort_values(ascending=False)

print("Yearly average total counts:")
for year, avg_count in yearly_avg_counts.items():
    print(f"{year}: {avg_count:,.2f}")

# Find most active year(s)
max_avg = yearly_avg_counts.max()
most_active_years = yearly_avg_counts[yearly_avg_counts == max_avg].index.tolist()
print(f"\nMost active year(s): {most_active_years} with average total counts: {max_avg:,.2f}")

# Activity 1e: Most active month(s) during the most active year
print("\n" + "="*60)
print("1e. Most active months in the most active year(s):")

# Extract month from date
df_cleaned.loc[:, 'month'] = pd.to_datetime(df_cleaned['date'], format='%d-%b-%y').dt.month
for year in most_active_years:
    year_data = df_cleaned[df_cleaned['year'] == year]
    monthly_activity = year_data.groupby('month').size().sort_values(ascending=False)
    
    print(f"\nYear {year} - Monthly activity (number of flares):")
    for month, count in monthly_activity.items():
        month_name = datetime(2000, month, 1).strftime('%B')
        print(f"  {month_name}: {count} flares")
    
    most_active_month = monthly_activity.idxmax()
    max_count = monthly_activity.max()
    month_name = datetime(2000, most_active_month, 1).strftime('%B')
    print(f"Most active month: {month_name} with {max_count} flares")

# Activity 1f: Flags mostly present in each solar flare class
print("\n" + "="*60)
print("1f. Most frequent flags by solar flare class (Top 3 per class):")

def extract_flags(flags_string):
    """Extract individual flags from flags string"""
    if pd.isna(flags_string):
        return []
    return flags_string.split()

# Analyze flags for each flare class
all_classes = df_cleaned['class_str'].unique()

for class_name in sorted(all_classes):
    class_data = df_cleaned[df_cleaned['class_str'] == class_name]
    
    if len(class_data) > 0:
        flag_counter = {}
        
        for flags in class_data['flags']:
            individual_flags = extract_flags(flags)
            for flag in individual_flags:
                flag_counter[flag] = flag_counter.get(flag, 0) + 1
        
        # Get top 3 flags
        top_flags = sorted(flag_counter.items(), key=lambda x: x[1], reverse=True)[:3]
        
        print(f"\n{class_name} class ({len(class_data)} events):")  # Fixed: changed {class} to {class_name}
        if top_flags:
            for flag, count in top_flags:
                percentage = (count / len(class_data)) * 100
                print(f"  {flag}: {count} occurrences ({percentage:.1f}%)")
        else:
            print("  No flags found")