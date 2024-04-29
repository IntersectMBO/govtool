type Props = {
  text: string;
  iconSrc: string;
  onClick?: () => void;
};

export const CopyButton = ({ iconSrc, onClick }: Props) => {
  return (
    <img
      data-testid="copy-button"
      alt="copy"
      onClick={onClick}
      src={iconSrc}
      style={{ cursor: "pointer" }}
    />
  );
};
