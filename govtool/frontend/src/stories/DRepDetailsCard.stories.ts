import { ComponentProps } from "react";
import type { Meta, StoryObj } from "@storybook/react";
import { DRepData, DRepStatus, MetadataValidationStatus } from "@models";
import { DRepDetailsCard } from "@organisms";

const meta = {
  title: "Example/DRepDetailsCard",
  component: DRepDetailsCard,
  parameters: {
    layout: "centered",
  },
  args: {
    isConnected: true,
    dRepData: {
      deposit: 1000000,
      drepId: "exampleDrepId",
      latestRegistrationDate: new Date().toISOString(),
      status: DRepStatus.Active,
      type: "DRep",
      url: "https://exampleurl.com",
      view: "exampleView",
      votingPower: 1000000,
      paymentAddress: "examplePaymentAddress",
      givenName: "John Smith",
      objectives: "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Vestibulum ornare pellentesque hendrerit. Pellentesque et placerat ex. Curabitur vitae pharetra ligula. Nullam euismod, odio sit amet suscipit facilisis, neque erat ultricies velit, sed rutrum neque nisi ac dui. Donec lobortis metus pulvinar varius gravida. Duis blandit, tortor non placerat commodo, metus lorem aliquam augue, eget ultricies nunc massa eu arcu. Etiam pellentesque urna nisl, facilisis placerat elit congue quis. Nulla quis dolor ac eros ullamcorper convallis ac ut enim. Proin faucibus urna at mi blandit, ut gravida sapien lacinia. Sed pretium, magna non tempor sollicitudin, tellus odio efficitur enim, ut feugiat nisi ligula non dolor. Fusce volutpat condimentum arcu, eu tempus neque convallis non. Suspendisse mattis sit amet libero et fringilla. Suspendisse eget erat eu nisl feugiat varius. Interdum et malesuada fames ac ante ipsum primis in faucibus. Curabitur vehicula eleifend lectus, vel eleifend felis vestibulum.",
      motivations: "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Cras semper tortor ullamcorper volutpat vehicula. Duis varius orci a elit luctus, in fringilla nisl fringilla. Fusce pellentesque convallis dapibus. In hac habitasse platea dictumst. Nunc efficitur ipsum at ipsum blandit, ac eleifend purus pulvinar. Pellentesque orci quam, interdum eget massa id, sollicitudin lacinia turpis. Nullam lectus quam, congue commodo sollicitudin in, pretium sit amet metus. Integer pretium, odio eu dictum posuere.",
      qualifications: "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc porta iaculis sodales. Praesent non nisi fermentum, porta sem in, porta arcu. In dignissim pulvinar est eu dignissim. Duis vitae vehicula dui. Praesent posuere egestas lacus, at pulvinar elit tempus ut. Etiam vulputate, lorem in accumsan.",
      references: ["https://example.com/", "exampleReference2"],
      doNotList: false,
      metadataStatus: null,
      metadataValid: true,
    } as DRepData,
  },
  tags: ["autodocs"],
} satisfies Meta<ComponentProps<typeof DRepDetailsCard>>;

export default meta;

type Story = StoryObj<typeof meta>;

export const MeAsDRep: Story = {
  args: {
    variant: "meAsDRep",
  },
};

export const MyDRep: Story = {
  args: {
    variant: "myDRep",
  },
};

export const MyDRepInProgress: Story = {
  args: {
    variant: "myDRepInProgress",
  },
};

export const OtherDRep: Story = {
  args: {
    variant: "default",
  },
};

export const UserNotConnected: Story = {
  args: {
    isConnected: false,
    variant: "default",
  },
};

export const InvalidData: Story = {
  args: {
    variant: "default",
    dRepData: {
      ...meta.args.dRepData,
      metadataStatus: MetadataValidationStatus.INCORRECT_FORMAT,
    },
  },
};
