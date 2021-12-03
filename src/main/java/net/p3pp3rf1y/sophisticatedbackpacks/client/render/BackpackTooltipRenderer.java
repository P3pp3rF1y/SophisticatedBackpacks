package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.Tesselator;
import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraftforge.client.event.RenderTooltipEvent;
import net.minecraftforge.event.world.WorldEvent;
import net.minecraftforge.fluids.FluidStack;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TextureBlitData;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.network.RequestBackpackInventoryContentsMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.util.CountAbbreviator;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.UUID;

public class BackpackTooltipRenderer {

	private static final String BACKPACK_ITEM_NAME = "backpack";

	private BackpackTooltipRenderer() {}

	private static final int REFRESH_INTERVAL = 20;
	private static boolean shouldRefreshContents = true;
	private static long lastRequestTime = 0;
	private static ContentsTooltipPart contentsTooltipPart;
	@Nullable
	private static UUID backpackUuid = null;

	@SuppressWarnings("unused") //parameter needs to be there so that addListener logic would know which event this method listens to
	public static void onWorldLoad(WorldEvent.Load event) {
		shouldRefreshContents = true;
		lastRequestTime = 0;
	}

	public static void handleBackpackTooltipRender(RenderTooltipEvent.Pre event) {
		ItemStack backpack = event.getStack();
		Minecraft minecraft = Minecraft.getInstance();
		LocalPlayer player = minecraft.player;
		if (!(backpack.getItem() instanceof BackpackItem) || !Screen.hasShiftDown() || player == null) {
			return;
		}
		if (renderBackpackTooltip(backpack, minecraft, player, event.getMatrixStack(), event.getX(), event.getY(), event.getFontRenderer())) {
			event.setCanceled(true);
		}
	}

	public static boolean renderBackpackTooltip(ItemStack backpack, Minecraft minecraft, LocalPlayer player, PoseStack poseStack, int x, int y, Font font) {
		return backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).map(wrapper -> {
			initContents(minecraft, player, wrapper);

			List<Component> lines = backpack.getTooltipLines(player, minecraft.options.advancedItemTooltips ? TooltipFlag.Default.ADVANCED : TooltipFlag.Default.NORMAL);
			if (backpackUuid != null) {
				int multiplier = wrapper.getInventoryHandler().getStackSizeMultiplier();
				if (multiplier > 1) {
					lines.add(new TranslatableComponent("item.sophisticatedbackpacks.backpack.tooltip.stack_multiplier",
							new TextComponent(Integer.toString(multiplier)).withStyle(ChatFormatting.WHITE)
					).withStyle(ChatFormatting.GREEN));
				}
				addEnergytooltip(wrapper, lines);
				addFluidTooltip(wrapper, lines);
			}
			renderContentsTooltip(backpack, minecraft, poseStack, x, y, font, lines);

			return true;
		}).orElse(false);
	}

	public static void renderTooltipWithContents(ItemStack backpack, Minecraft minecraft, PoseStack poseStack, int x, int y, Font font, List<Component> lines) {
		backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(wrapper -> {
			if (minecraft.player != null) {
				initContents(minecraft, minecraft.player, wrapper);
				renderContentsTooltip(backpack, minecraft, poseStack, x, y, font, lines);
			}
		});
	}

	private static void renderContentsTooltip(ItemStack backpack, Minecraft minecraft, PoseStack poseStack, int x, int y, Font font, List<Component> lines) {
		GuiHelper.renderTooltip(minecraft, poseStack, lines, x, y, contentsTooltipPart, font, backpack);
	}

	private static void initContents(Minecraft minecraft, LocalPlayer player, IBackpackWrapper wrapper) {
		UUID newUuid = wrapper.getContentsUuid().orElse(null);
		if (backpackUuid == null && newUuid != null || backpackUuid != null && !backpackUuid.equals(newUuid)) {
			lastRequestTime = 0;
			backpackUuid = newUuid;
			shouldRefreshContents = true;
		}
		requestContents(player, wrapper);
		refreshContents(wrapper, minecraft);
	}

	private static void addEnergytooltip(IBackpackWrapper wrapper, List<Component> lines) {
		wrapper.getEnergyStorage().ifPresent(energyStorage -> lines.add(new TranslatableComponent(TranslationHelper.translItemTooltip(BACKPACK_ITEM_NAME) + ".energy",
				new TextComponent(CountAbbreviator.abbreviate(energyStorage.getEnergyStored())).withStyle(ChatFormatting.WHITE)).withStyle(ChatFormatting.RED)
		));
	}

	private static void addFluidTooltip(IBackpackWrapper wrapper, List<Component> lines) {
		wrapper.getFluidHandler().ifPresent(fluidHandler -> {
			for (int tank = 0; tank < fluidHandler.getTanks(); tank++) {
				FluidStack fluid = fluidHandler.getFluidInTank(tank);
				if (fluid.isEmpty()) {
					lines.add(new TranslatableComponent(TranslationHelper.translItemTooltip(BACKPACK_ITEM_NAME) + ".fluid_empty").withStyle(ChatFormatting.BLUE));
				} else {
					lines.add(new TranslatableComponent(TranslationHelper.translItemTooltip(BACKPACK_ITEM_NAME) + ".fluid",
							new TextComponent(CountAbbreviator.abbreviate(fluid.getAmount())).withStyle(ChatFormatting.WHITE),
							new TranslatableComponent(fluid.getTranslationKey()).withStyle(ChatFormatting.BLUE)

					));
				}
			}
		});
	}

	private static void requestContents(LocalPlayer player, IBackpackWrapper wrapper) {
		if (lastRequestTime + REFRESH_INTERVAL < player.level.getGameTime()) {
			lastRequestTime = player.level.getGameTime();
			wrapper.getContentsUuid().ifPresent(uuid -> PacketHandler.sendToServer(new RequestBackpackInventoryContentsMessage(uuid)));
		}
	}

	private static void refreshContents(IBackpackWrapper wrapper, Minecraft minecraft) {
		if (shouldRefreshContents) {
			shouldRefreshContents = false;
			if (backpackUuid != null) {
				wrapper.onContentsNbtUpdated();
				List<ItemStack> sortedContents = InventoryHelper.getCompactedStacksSortedByCount(wrapper.getInventoryHandler());
				contentsTooltipPart = new ContentsTooltipPart(minecraft, new TreeMap<>(wrapper.getUpgradeHandler().getSlotWrappers()), sortedContents);
			} else {
				contentsTooltipPart = getEmptyInventoryTooltip(minecraft);
			}
		}
		if (contentsTooltipPart == null) {
			contentsTooltipPart = getEmptyInventoryTooltip(minecraft);
		}
	}

	private static ContentsTooltipPart getEmptyInventoryTooltip(Minecraft minecraft) {
		return new ContentsTooltipPart(minecraft, new HashMap<>(), new ArrayList<>());
	}

	//TODO this probably needs to move somewhere else, but there's no easy way to understand what STACK requested refresh of contents and tooltip is the only one at the moment
	public static void refreshContents() {
		shouldRefreshContents = true;
	}

	private static class ContentsTooltipPart implements GuiHelper.ITooltipRenderPart {
		private static final TextureBlitData UPGRADE_ON = new TextureBlitData(GuiHelper.ICONS, Dimension.SQUARE_256, new UV(4, 128), Dimension.RECTANGLE_4_10);
		private static final TextureBlitData UPGRADE_OFF = new TextureBlitData(GuiHelper.ICONS, Dimension.SQUARE_256, new UV(0, 128), Dimension.RECTANGLE_4_10);
		private static final int MAX_STACKS_ON_LINE = 9;
		private static final int DEFAULT_STACK_WIDTH = 18;
		private static final int COUNT_PADDING = 2;
		private final Minecraft minecraft;
		private final Map<Integer, IUpgradeWrapper> upgrades;
		private final List<ItemStack> backpackContents;
		private int height;
		private int width;

		public ContentsTooltipPart(Minecraft minecraft, Map<Integer, IUpgradeWrapper> upgrades, List<ItemStack> backpackContents) {
			this.minecraft = minecraft;
			this.upgrades = upgrades;
			this.backpackContents = backpackContents;
			calculateHeight();
			calculateWidth();
		}

		private void calculateWidth() {
			int upgradesWidth = calculateUpgradesWidth();
			int contentsWidth = calculateContentsWidth();
			int stacksWidth = Math.max(upgradesWidth, contentsWidth);
			width = stacksWidth > 0 ? stacksWidth : getEmptyTooltipWidth();
		}

		private int calculateUpgradesWidth() {
			int upgradesWidth = 0;
			for (IUpgradeWrapper upgradeWrapper : upgrades.values()) {
				upgradesWidth += (upgradeWrapper.canBeDisabled() ? 4 : 0) + DEFAULT_STACK_WIDTH;
			}
			return upgradesWidth;
		}

		private int calculateContentsWidth() {
			Font fontRenderer = Minecraft.getInstance().font;
			int contentsWidth = 0;
			for (int i = 0; i < backpackContents.size() && i < MAX_STACKS_ON_LINE; i++) {
				int countWidth = getStackCountWidth(fontRenderer, backpackContents.get(i));
				contentsWidth += Math.max(countWidth, DEFAULT_STACK_WIDTH);
			}

			return contentsWidth;
		}

		private int getStackCountWidth(Font fontRenderer, ItemStack stack) {
			return fontRenderer.width(CountAbbreviator.abbreviate(stack.getCount())) + COUNT_PADDING;
		}

		private void calculateHeight() {
			int upgradesHeight = upgrades.isEmpty() ? 0 : 32;
			int inventoryHeight = backpackContents.isEmpty() ? 0 : 12 + (1 + (backpackContents.size() - 1)/ MAX_STACKS_ON_LINE) * 20;
			int totalHeight = upgradesHeight + inventoryHeight;
			height = totalHeight > 0 ? totalHeight : 12;
		}

		@Override
		public int getWidth() {
			return width;
		}

		private int getEmptyTooltipWidth() {
			return Minecraft.getInstance().font.width(new TranslatableComponent(BackpackItem.BACKPACK_TOOLTIP + "empty").getVisualOrderText());
		}

		@Override
		public int getHeight() {
			return height;
		}

		@Override
		public void render(PoseStack matrixStack, int leftX, int topY, Font font) {
			if (!upgrades.isEmpty()) {
				topY = renderTooltipLine(leftX, topY, matrixStack, font, "upgrades");
				topY = renderUpgrades(matrixStack, leftX, topY);
			}
			if (!backpackContents.isEmpty()) {
				topY = renderTooltipLine(leftX, topY, matrixStack, font, "inventory");
				renderContents(matrixStack, leftX, topY);
			}
			if (upgrades.isEmpty() && backpackContents.isEmpty()) {
				renderTooltipLine(leftX, topY, matrixStack, font, "empty");
			}
		}

		private int renderTooltipLine(int leftX, int topY, PoseStack matrixStack, Font font, String tooltip) {
			MultiBufferSource.BufferSource renderTypeBuffer = MultiBufferSource.immediate(Tesselator.getInstance().getBuilder());
			topY = GuiHelper.writeTooltipLines(Collections.singletonList(new TranslatableComponent(BackpackItem.BACKPACK_TOOLTIP + tooltip).withStyle(ChatFormatting.YELLOW)),
					font, leftX, topY, matrixStack.last().pose(), renderTypeBuffer, -1);
			renderTypeBuffer.endBatch();
			return topY;
		}

		private int renderUpgrades(PoseStack matrixStack, int leftX, int topY) {
			int x = leftX;
			for (IUpgradeWrapper upgradeWrapper : upgrades.values()) {
				if (upgradeWrapper.canBeDisabled()) {
					GuiHelper.blit(minecraft, matrixStack, x, topY + 3, upgradeWrapper.isEnabled() ? UPGRADE_ON : UPGRADE_OFF);
					x += 4;
				}
				GuiHelper.renderItemInGUI(matrixStack, minecraft, upgradeWrapper.getUpgradeStack(), x, topY, true);
				x += DEFAULT_STACK_WIDTH;
			}
			topY += 20;
			return topY;
		}

		private void renderContents(PoseStack matrixStack, int leftX, int topY) {
			int x = leftX;
			for (int i = 0; i < backpackContents.size(); i++) {
				int y = topY + i / MAX_STACKS_ON_LINE * 20;
				if (i % MAX_STACKS_ON_LINE == 0) {
					x = leftX;
				}
				ItemStack stack = backpackContents.get(i);
				int stackWidth = Math.max(getStackCountWidth(minecraft.font, stack), DEFAULT_STACK_WIDTH);
				int xOffset = stackWidth - DEFAULT_STACK_WIDTH;
				GuiHelper.renderItemInGUI(matrixStack, minecraft, stack, x + xOffset, y, true, CountAbbreviator.abbreviate(stack.getCount()));
				x += stackWidth;
			}
		}
	}
}
