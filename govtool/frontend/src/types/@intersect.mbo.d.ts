type PDFProps = {
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  walletAPI: any;
  pathname: string;
  locale?: string;
};

declare module "@intersect.mbo/pdf-ui/cjs" {
  export default function PDF(props: PDFProps): JSX.Element;
}
