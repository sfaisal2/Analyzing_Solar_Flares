import pandas as pd, numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import os
from sklearn.model_selection import TimeSeriesSplit, GridSearchCV
from sklearn.tree import DecisionTreeClassifier, export_text
from sklearn.metrics import classification_report, confusion_matrix
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline

df = pd.read_csv('RHESSI_SolarFlares_SolarCycle24_Task3_2025.csv', sep=';')

mask = ~(((df.x_pos==0)&(df.y_pos==0)) & (df.energy_band.astype(str).str.replace(' ','').str.startswith('3-6')))
df = df.loc[mask].copy()

y = df['class_str'].map({'NF':0,'C':1,'M':2,'X':3}).values

X = df[['duration','total_counts','peak_counts_per_sec','radial']].copy()
X = X.fillna(X.median(numeric_only=True))

df['date'] = pd.to_datetime(df['date'], errors='coerce')
order = np.argsort(df['date'].values)
X, y = X.iloc[order], y[order]

split = int(len(X)*0.8)
Xtr, Xte = X.iloc[:split], X.iloc[split:]
ytr, yte = y[:split], y[split:]

pipe = Pipeline([
    ('scaler', StandardScaler()),
    ('dt', DecisionTreeClassifier(random_state=0))
])

param_grid = {
    'dt__criterion': ['gini','entropy'],
    'dt__max_depth': [3,4,5,6,7,8],
    'dt__min_samples_leaf': [5,10,20],
    'dt__max_leaf_nodes': [8,12,16,20,24,28,30]
}

tscv = TimeSeriesSplit(n_splits=5)
g = GridSearchCV(pipe, param_grid, cv=tscv, n_jobs=-1, scoring='f1_macro', refit=True)
g.fit(Xtr, ytr)

best_idx = g.best_index_
mean_cv = g.cv_results_['mean_test_score'][best_idx]
std_cv  = g.cv_results_['std_test_score'][best_idx]

print("CV best params:", g.best_params_)
print(f"Best CV macro F1 mean ± std: {mean_cv:.3f} ± {std_cv:.3f}")

best_model = g.best_estimator_
yp = best_model.predict(Xte)

print(classification_report(yte, yp, digits=3, zero_division=0))
print(confusion_matrix(yte, yp))

dt = best_model.named_steps['dt']
print(export_text(dt, feature_names=list(X.columns), max_depth=3))

cm = confusion_matrix(yte, yp)
plt.figure(figsize=(4,3))
sns.heatmap(cm, annot=True, fmt='d', cmap='Blues',
            xticklabels=['NF','C','M','X'],
            yticklabels=['NF','C','M','X'])
plt.xlabel('Predicted'); plt.ylabel('True'); plt.tight_layout()
os.makedirs("figures", exist_ok=True)
plt.savefig('figures/confusion_matrix.png', dpi=200)

