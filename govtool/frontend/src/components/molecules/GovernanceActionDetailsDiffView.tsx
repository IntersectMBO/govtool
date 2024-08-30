import { diffLines, formatLines } from "unidiff";
import { parseDiff, Diff, Hunk } from "react-diff-view";
import "react-diff-view/style/index.css";
import "./react-diff-view.overrides.css";

type Props = {
  oldJson?: JSON | Record<string, unknown> | null;
  newJson?: JSON | Record<string, unknown> | null;
};

export const GovernanceActionDetailsDiffView = ({
  oldJson,
  newJson,
}: Props) => {
  const diffText = formatLines(
    diffLines(
      JSON.stringify(oldJson, null, 2),
      JSON.stringify(newJson, null, 2),
    ),
  );

  const [diff] = parseDiff(diffText, {});

  if (!oldJson && !newJson) return;

  return (
    <Diff viewType="split" diffType={diff.type} hunks={diff.hunks || []}>
      {(hunks) =>
        hunks.map((hunk) => (
          // Hunk component does not allow to pass children as a prop
          // but that is the typing issue as passing the children
          // is recommended by documentation approach.
          // eslint-disable-next-line @typescript-eslint/ban-ts-comment
          // @ts-expect-error
          <Hunk key={hunk.content} hunk={hunk}>
            {hunk.content}
          </Hunk>
        ))
      }
    </Diff>
  );
};
