import tabula
import pandas as pd

pdf_path = 'Dropbox/Projects/MoBaDataDictionary/Q_PDF/'
pdf_doc  = 'q08-y07-m.pdf'


# Read tables from the PDF using tabula
tables = tabula.read_pdf(pdf_path, pages='all', multiple_tables=True)

# Create a new Excel file and save the tables in separate sheets
with pd.ExcelWriter('output.xlsx') as writer:
    for i, table in enumerate(tables, start=1):
        table.to_excel(writer, sheet_name=f'Table_{i}', index=False)

print("Tables extracted and saved as 'output.xlsx'")

