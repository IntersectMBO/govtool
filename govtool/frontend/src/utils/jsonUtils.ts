export const downloadJson = (json: string, fileName?: string) => {
  const jsonString = `data:text/json;chatset=utf-8,${encodeURIComponent(json)}`;
  const link = document.createElement("a");
  link.href = jsonString;
  link.download = `${fileName ? fileName : "data"}.json`;

  link.click();
};
