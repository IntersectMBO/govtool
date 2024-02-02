export interface BasicReducer<T> {
  (prevState: T, newState: Partial<T>): T;
}

export const basicReducer = <T>(prevState: T, newState: Partial<T>): T => ({
  ...prevState,
  ...newState,
});
