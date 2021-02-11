import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np

# Import data
df = pd.read_csv("medical_examination.csv")

# Add 'overweight' column
df["overweight"] = df.apply(
    lambda row: 0 if row.weight / np.square(row.height / 100) < 25 else 1, axis=1
)

# Normalize data by making 0 always good and 1 always bad. If the value of 'cholestorol' or 'gluc' is 1, make the value 0. If the value is more than 1, make the value 1.

df["cholesterol"] = df.apply(lambda row: 0 if row.cholesterol == 1 else 1, axis=1)
df["gluc"] = df.apply(lambda row: 0 if row.gluc == 1 else 1, axis=1)


# Draw Categorical Plot
def draw_cat_plot():
    # Create DataFrame for cat plot using `pd.melt` using just the values from 'cholesterol', 'gluc', 'smoke', 'alco', 'active', and 'overweight'.
    df_cat = pd.melt(
        df,
        id_vars=["cardio"],
        value_vars=["cholesterol", "gluc", "smoke", "alco", "active", "overweight"],
    )

    # Group and reformat the data to split it by 'cardio'. Show the counts of each feature. You will have to rename one of the columns for the catplot to work correctly.
    df_cat = (
        df_cat.groupby(["cardio", "variable", "value"]).size().reset_index(name="total")
    )

    # Draw the catplot with 'sns.catplot()'
    fig = sns.catplot(
        x="variable", y="total", hue="value", col="cardio", kind="bar", data=df_cat
    )

    # Do not modify the next two lines
    fig.savefig("catplot.png")
    return fig


# Draw Heat Map
def draw_heat_map():
    # Clean the data
    df_heat = df.drop(
        df[
            (df.ap_lo > df.ap_hi)
            | (df.height < df["height"].quantile(0.025))
            | (df.height > df["height"].quantile(0.975))
            | (df.weight < df["weight"].quantile(0.025))
            | (df.weight > df["weight"].quantile(0.975))
        ].index
    )

    # Calculate the correlation matrix
    corr = df_heat.corr().round(1)

    # Generate a mask for the upper triangle
    mask = np.triu(np.ones(corr.shape)).astype(np.bool)

    # Set up the matplotlib figure
    fig, ax = plt.subplots()

    # Draw the heatmap with 'sns.heatmap()'
    ax = sns.heatmap(corr, mask=mask, annot=True, fmt=".1f")

    # Do not modify the next two lines
    fig.savefig("heatmap.png")
    return fig
