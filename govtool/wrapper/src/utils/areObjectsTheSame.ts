export const areObjectsTheSame = (obj1: object, obj2: object) => {
  const obj1Props = Object.keys(obj1);
  const obj2Props = Object.keys(obj2);

  if (obj1Props.length !== obj2Props.length) {
    return false;
  }
  if (obj1Props.some((prop) => !obj2.hasOwnProperty(prop))) {
    return false;
  }

  return true;
};
