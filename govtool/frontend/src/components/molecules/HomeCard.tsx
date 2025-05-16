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
    }}
    onCardClick={onCardClick}
  >
    <Typography>{title}</Typography>
    <Typography variant="caption">{description}</Typography>
  </Card>
);
