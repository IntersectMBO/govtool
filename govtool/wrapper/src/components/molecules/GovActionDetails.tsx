import { Typography } from "../atoms";

export const GovActionDetails = ({
  title,
  value,
}: {
  title: string;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  value: any;
}) => {
  if (Array.isArray(value)) {
    return (
      <div>
        <Typography variant="caption">{title}:</Typography>
        <ul>
          {value.map((item, index) => (
            // TODO: Get rid of index as key - ref: https://github.com/jsx-eslint/eslint-plugin-react/blob/master/docs/rules/no-array-index-key.md
            // eslint-disable-next-line react/no-array-index-key
            <li key={index}>
              <Typography
                sx={{ textOverflow: "ellipsis", overflow: "hidden" }}
                variant="caption"
              >
                {item}
              </Typography>
            </li>
          ))}
        </ul>
      </div>
    );
  }
  if (typeof value === "boolean") {
    return (
      <Typography
        sx={{ textOverflow: "ellipsis", overflow: "hidden" }}
        variant="caption"
      >
        {title}:{value ? "True" : "False"}
      </Typography>
    );
  }
  return (
    <Typography
      sx={{ textOverflow: "ellipsis", overflow: "hidden" }}
      variant="caption"
    >
      {title}:{value}
    </Typography>
  );
};
