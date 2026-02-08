import { expect, test } from "@playwright/test";

type StrategyInfo = { id: string; name: string };
type TreeNode = { id: string; owner_id: string; parent_tree_id: string | null };

test("selecting different strategy nodes updates the right title", async ({ page, request }) => {
  const created = await request.get("/StrategyCreate");
  expect(created.ok()).toBeTruthy();
  const rootStrategy = (await created.json()) as StrategyInfo;

  const readRoot = await request.get(`/StrategyTreeRead?strategyId=${rootStrategy.id}`);
  expect(readRoot.ok()).toBeTruthy();
  const rootNodes = (await readRoot.json()) as TreeNode[];
  expect(rootNodes.length).toBeGreaterThan(0);
  const rootTreeId = rootNodes[0].id;

  const createChild = await request.get(
    `/StrategyTreeCreate?strategyId=${rootStrategy.id}&parentTreeId=${rootTreeId}`
  );
  expect(createChild.ok()).toBeTruthy();
  const childNode = (await createChild.json()) as TreeNode;
  const childStrategyId = childNode.owner_id;

  const childStrategyResp = await request.get(`/StrategyRead?strategyId=${childStrategyId}`);
  expect(childStrategyResp.ok()).toBeTruthy();
  const childStrategy = (await childStrategyResp.json()) as StrategyInfo;

  await page.goto(`/Strategy?strategyId=${rootStrategy.id}`);
  await page.waitForSelector("[data-tree-root] span");
  await expect(page.locator("[data-cell-title='1']")).toHaveText("策略");

  const nodeSpans = page.locator("[data-tree-root] span");
  await expect(nodeSpans).toHaveCount(3);

  await nodeSpans.nth(1).click();
  await expect(page.locator("[data-cell-title='1']")).toHaveText(rootStrategy.name);

  await nodeSpans.nth(2).click();
  await expect(page.locator("[data-cell-title='1']")).toHaveText(childStrategy.name);
  expect(childStrategy.name).not.toEqual(rootStrategy.name);
});
