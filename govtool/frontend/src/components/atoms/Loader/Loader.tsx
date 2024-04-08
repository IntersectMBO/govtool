import { ICONS } from "@/consts";
import "./Loader.css";

type Props = {
  size: number;
};
export const Loader = ({ size = 100 }: Props) => (
  <div style={{ width: size, height: size, margin: "0 auto" }}>
    <img
      src={ICONS.loaderIcon}
      style={{ animation: `spin 2s linear infinite` }}
      alt="loader"
    />
  </div>
);
