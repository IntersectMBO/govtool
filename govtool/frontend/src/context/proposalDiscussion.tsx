import {
  PropsWithChildren,
  useMemo,
  createContext,
  useContext,
  useState,
} from "react";

type ProposalDiscussionContextType = {
  username: string;
  setUsername: (username: string) => void;
} | null;

const ProposalDiscussionContext =
  createContext<ProposalDiscussionContextType>(null);

/**
 * Provides proposal discussion context to its children components.
 *
 * @param children - The child components to render.
 */
const ProposalDiscussionProvider = ({ children }: PropsWithChildren) => {
  const [username, setUsername] = useState<string>("");

  const value = useMemo(
    () => ({
      username,
      setUsername,
    }),
    [username],
  );

  return (
    <ProposalDiscussionContext.Provider value={value}>
      {children}
    </ProposalDiscussionContext.Provider>
  );
};

/**
 * Custom hook to use the ProposalDiscussionContext.
 * @returns The context value.
 */
const useProposalDiscussion = () => {
  const context = useContext(ProposalDiscussionContext);
  if (!context) {
    throw new Error(
      "useProposalDiscussion must be used within a ProposalDiscussionProvider",
    );
  }
  return context;
};

export { ProposalDiscussionProvider, useProposalDiscussion };
