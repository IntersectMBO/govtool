import { Card } from "./Card";
import { Typography } from "../atoms";

type Props = {
  title: string;
  description: string;
  onCardClick?: () => void;
};

export const HomeCard = ({ title, description, onCardClick }: Props) => (
  <Card
    sx={{
      flexBasis: 0,
      boxShadow: "2px 2px 20px 0px rgba(47, 98, 220, 0.20)",
      boxSizing: "border-box",
      display: "flex",
      flexDirection: "column",
      gap: 1,
      border: "none",
      cursor: onCardClick ? "pointer" : "default",
      outline: "none",
      p: 3.75,
      "&:focus": {
        boxShadow: "0 0 0 3px rgba(47, 98, 220, 0.5)",
      },
    }}
    onCardClick={onCardClick}
    component="button"
    role="button"
    aria-label={`${title}. ${description}`}
    dataTestId={`home-card-${title.replace(/\s+/g, "-").toLowerCase()}`}
  >
    <Typography component="h3" variant="titleH3">
      {title}
    </Typography>
    <Typography variant="body">{description}</Typography>
  </Card>
);
