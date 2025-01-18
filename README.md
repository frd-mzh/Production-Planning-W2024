# Production Planning and Forecasting

This repository contains the project **Production Planning (Phase 1, 2)**, focused on optimizing production and inventory planning using data-driven techniques. The project was developed as part of an academic course project under supervision of **Dr. Hadi Mossadegh** during Fall 2024.

## Project Overview

The project is divided into two phases:
1. **Phase 1**:
   - Conducted sales forecasting for the automotive industry in Japan, the USA, and Germany using various statistical methods.
   - Compared forecasting methods such as:
     - Simple Moving Average (SMA)
     - Weighted Moving Average (WMA)
     - Single Exponential Smoothing (SES)
     - Linear Regression
   - Assessed models using performance metrics like **Tracking Signal**.
   - Implemented forecasting and optimization models using Python and the `cvxpy` library.

2. **Phase 2**:
   - Calculated **Economic Production Quantity (EPQ)** for efficient production planning.
   - Developed **MPS** (Master Production Schedule) and **MRP** (Material Requirements Planning) tables based on EPQ calculations.

## Features

- **Forecasting Techniques**: Compare multiple statistical forecasting models to predict future sales.
- **Optimization Models**: Determine the optimal production quantities and schedules to minimize costs.
- **Sensitivity Analysis**: Evaluate the impact of varying production parameters on the objective function.
- **Inventory Control**: Plan and optimize stock levels across various time periods.

## Project Structure

```
.
├── Phase1/
│   ├── phase1_report.pdf            # Report for Phase 1
│   ├── phase1_forecasting.py        # Code for sales forecasting
│   └── data_phase1.csv              # Dataset used in Phase 1
├── Phase2/
│   ├── phase2_report.pdf            # Report for Phase 2
│   ├── phase2_optimization.py       # Code for production optimization
│   └── data_phase2.csv              # Dataset used in Phase 2
├── README.md                        # Project documentation
```

## Usage

- Modify the input datasets (`data_phase1.csv`) as needed for different scenarios.
- Run the Python scripts for each phase to reproduce results and visualize the findings.
- Refer to the PDFs for detailed documentation of the methodology and results.

## Tools and Technologies

- **Python**: Primary programming language
- **cvxpy**: Optimization library
- **Matplotlib** and **Seaborn**: Data visualization
- **Pandas** and **NumPy**: Data manipulation

## Authors

- **Farid Mohammadzadeh**

