import { PropsWithChildren, useMemo, createContext, useContext } from "react";

const FeatureFlagContext = createContext({
  isProposalDiscussionForumEnabled: false,
});

/**
 * Provides feature flag context to its children components.
 *
 * @param children - The child components to render.
 */
const FeatureFlagProvider = ({ children }: PropsWithChildren) => {
  const value = useMemo(
    () => ({
      isProposalDiscussionForumEnabled:
        import.meta.env.VITE_IS_PROPOSAL_DISCUSSION_FORUM_ENABLED === "true" ||
        false,
    }),
    [],
  );

  return (
    <FeatureFlagContext.Provider value={value}>
      {children}
    </FeatureFlagContext.Provider>
  );
};

/**
 * Custom hook that provides access to the feature flag context.
 * Throws an error if used outside of a FeatureFlagProvider.
 * @returns The feature flag context.
 */
const useFeatureFlag = () => {
  const context = useContext(FeatureFlagContext);

  if (!context) {
    throw new Error("useFeatureFlag must be used within a FeatureFlagProvider");
  }

  return context;
};

export { FeatureFlagProvider, useFeatureFlag };
