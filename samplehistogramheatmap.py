import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

df = pd.read_csv("data.csv")

# Chỉ tính tương quan cho các cột chứa dữ liệu số, dùng lệnh này thì giá trị string sẽ được nhận
df_numeric = df.select_dtypes(include=["number"])
corr = df_numeric.iloc[:, :12].corr()

plt.figure(figsize=(10, 8))
sns.heatmap(corr, annot=True, fmt=".2f", cmap="coolwarm", vmin=-1, vmax=1)
plt.title(
    "Ma Trận Mối Liên Hệ Giữa Các Yếu Tố Số (Correlation Heatmap)",
    fontweight="bold",
)
plt.tight_layout()
plt.show()
