import { useScreenDimension } from "@hooks";
import { FC } from "react";
import { Typography, PagePaddingBox, ContentBox } from "@/components/atoms";

interface PageTitleProps {
  title: string;
}

export const PageTitle: FC<PageTitleProps> = ({ title }) => {
  const { isMobile } = useScreenDimension();

  return (
    <PagePaddingBox
      borderBottom={(theme) => `1px solid ${theme.palette.neutralWhite}`}
      py={3}
    >
      <ContentBox>
        <Typography variant={isMobile ? "title1" : "headline5"}>
          {title}
        </Typography>
      </ContentBox>
    </PagePaddingBox>
  );
};
