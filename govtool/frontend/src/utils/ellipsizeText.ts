export const ellipsizeText = (
  text: string,
  maxLength: number,
  ellipsize = "...",
) => {
  if (text.length <= maxLength) {
    return text;
  }
  const slicedText = text.slice(0, maxLength);

  return `${slicedText.trimEnd()}${ellipsize}`;
};
