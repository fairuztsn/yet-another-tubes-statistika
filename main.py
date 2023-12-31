from typing import List, Tuple, Optional
import pandas as pd
import numpy as np
import random

def augment(
    df: pd.DataFrame, 
    context: str, 
    n: int, 
    height_range: Tuple[int, int], 
    width_range: Tuple[int, int], 
    veins_range: Tuple[int, int] = (0, 0), 
    lobes_range: Tuple[int, int] = (0, 0),
    require: Optional[str] = "",
    to_csv: Optional[bool] = False
) -> pd.DataFrame:
    """
    Augment the DataFrame by generating new rows based on specified parameters.

    Parameters:
    - df (pd.DataFrame): The original DataFrame.
    - context (str): The plant context for the new rows.
    - n (int): Number of rows to generate.
    - height_range (Tuple[int, int]): Range for generating heights.
    - width_range (Tuple[int, int]): Range for generating widths.
    - veins_range (Tuple[int, int]): Range for generating veins count.
    - lobes_range (Tuple[int, int]): Range for generating lobes count.
    - require (Optional[str]): Requirement for height and width relationship ("l>w").
    - to_csv (Optional[bool]): Whether to save the DataFrame to CSV.

    Returns:
    - pd.DataFrame: The augmented DataFrame.
    """
    shape_map = {
        "snake plant": "lanceolate",
        "swiss cheese-plant": "elliptical",
        "plectranthus prostratus": "elliptical",
        "queen anthurium": "obcordate",
        "chinese evergreen": "oval",
        "bird's nest snake plant": "oval"
    }

    for _ in range(n):
        new_height, new_width = round(random.uniform(*height_range), 1), round(random.uniform(*width_range), 1)

        while require == "l>w" and np.floor(new_height) < np.floor(new_width):
            new_height, new_width = round(random.uniform(*height_range), 1), round(random.uniform(*width_range), 1)

        new_veins = round(random.uniform(*veins_range))

        if lobes_range != (0, 0):
            new_lobes = round(random.uniform(*lobes_range))
            new_sinus = max(0, new_lobes - 1)
        else:
            new_lobes, new_sinus = 0, 0

        new_row = pd.Series([context, new_height, new_width, np.nan, new_veins, new_lobes, new_sinus, shape_map[context]], index=df.columns)
        df = df._append(new_row, ignore_index=True)

    if to_csv:
        df.to_csv("augmented_data.csv", index=False)

    return df

df = pd.read_csv(input("Where's the dataset?\t"))

augment_params = {
    'context': input("Context?\t"),
    'n': int(input("How many rows to generate?\t")),
    'height_range': [int(input("Minimum height?\t")), int(input("Maximum height?\t"))],
    'width_range': [int(input("Minimum width?\t")), int(input("Maximum width?\t"))],
    'veins_range': [int(input("Minimum veins count?\t")), int(input("Maximum veins count?\t"))],
    'lobes_range': [int(input("Minimum lobes count?\t")), int(input("Maximum lobes count?\t"))],
    'require': input("Any requirements? ['l>w']\t"),
    'to_csv': input("Would you like to convert to csv? [y/n]\t").lower() == "y"
}

df = augment(df, **augment_params)

print(df)
