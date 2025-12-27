# Global AI Attitudes Report: 2025 🌍🤖

**Author:** Ramazan Karagöz - Eskişehir Technical University (Department of Statistics)  
**Data Source:** Pew Research Center Spring 2025 Global Attitudes Survey (n=25,000+)  
**Methodology:** Analysis and visualization performed using R (ggplot2) and R Markdown.

## 📂 Repository Contents

This repository is organized to provide both the final visual output and the technical reproducibility of the analysis.

### 1. Main Directory (Root)
* **`final_poster_A1.pdf`**: **Final A1 Print-Ready Poster.** This is the high-resolution version intended for physical presentation.

### 2. /poster
* **Knitted R Markdown Output**: Contains the PDF generated directly from R Markdown. This version includes both the **visualizations and the code chunks** used to create them, demonstrating the step-by-step analysis process.

### 3. /scripts
* **`final_analysis.R`**: A clean, standalone R script containing all visualization codes (ggplot2).
    * *Note:* To ensure the code runs seamlessly on any machine without path errors, the datasets are defined directly within this script.

### 4. /data
* **Aggregated Datasets**: Contains the raw `.csv` files (`ai_awareness_data.csv`, `ai_sentiment_data.csv`, `ai_trust_data.csv`) for reference.

---

## 📊 Quick Note on Reproducibility
For maximum compatibility and ease of review, the main R script (`/scripts/final_analysis.R`) includes the datasets directly within the code. You can simply run the script to reproduce all the plots used in the poster.
