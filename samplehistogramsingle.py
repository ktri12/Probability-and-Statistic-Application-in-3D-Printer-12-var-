import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

df = pd.read_csv("data.csv")

# Thiết lập khung vẽ 3 hàng, 4 cột (vừa đủ cho 12 biến)
fig, axes = plt.subplots(3, 4, figsize=(16, 10))
axes = axes.flatten()

# Lấy danh sách 12 cột (hoặc tự điền danh sách 12 tên cột của bạn)
columns = df.select_dtypes(include=["number"]).columns[:12]

for i, col in enumerate(columns):
    sns.histplot(df[col], bins=7, kde=True, ax=axes[i], color="skyblue")
    axes[i].set_title(col, fontsize=10, fontweight="bold")
    axes[i].set_xlabel("")
    axes[i].set_ylabel("")

plt.tight_layout()
plt.show()